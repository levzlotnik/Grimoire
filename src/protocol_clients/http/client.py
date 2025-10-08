"""HTTP client implementation using httpx"""
import httpx
from typing import Dict, List, Any, Optional
from dataclasses import dataclass


@dataclass
class HTTPEndpoint:
    """HTTP endpoint definition"""
    name: str
    path: str
    method: str


@dataclass
class HTTPService:
    """HTTP service with client and endpoints"""
    name: str
    base_url: str
    endpoints: List[HTTPEndpoint]
    client: httpx.Client


class HTTPServiceRegistry:
    """Registry for HTTP services with user-declared schemas"""

    def __init__(self):
        self.services: Dict[str, HTTPService] = {}

    def register_service(self, name: str, base_url: str, endpoints: List[Dict]) -> None:
        """Register a new HTTP service with endpoint schemas"""
        endpoint_objs = [
            HTTPEndpoint(e['name'], e['path'], e['method'])
            for e in endpoints
        ]

        client = httpx.Client(base_url=base_url, timeout=30.0)

        self.services[name] = HTTPService(
            name=name,
            base_url=base_url,
            endpoints=endpoint_objs,
            client=client
        )

    def call_endpoint(self, service_name: str, endpoint_name: str, params: Dict) -> httpx.Response:
        """Call a registered endpoint"""
        if service_name not in self.services:
            raise ValueError(f"Service {service_name} not registered")

        service = self.services[service_name]
        endpoint = next((e for e in service.endpoints if e.name == endpoint_name), None)

        if not endpoint:
            raise ValueError(f"Endpoint {endpoint_name} not found in {service_name}")

        # Substitute path parameters
        path = endpoint.path
        path_params = {k: v for k, v in params.items() if f':{k}' in path}
        for key, value in path_params.items():
            path = path.replace(f':{key}', str(value))

        # Remaining params are query/body params
        body_params = {k: v for k, v in params.items() if k not in path_params}

        method = endpoint.method.lower()
        if method == 'get':
            response = service.client.get(path, params=body_params)
        elif method == 'post':
            response = service.client.post(path, json=body_params)
        elif method == 'put':
            response = service.client.put(path, json=body_params)
        elif method == 'delete':
            response = service.client.delete(path, params=body_params)
        elif method == 'patch':
            response = service.client.patch(path, json=body_params)
        else:
            raise ValueError(f"Unsupported HTTP method: {method}")

        return response

    def list_services(self) -> List[HTTPService]:
        """List all registered services"""
        return list(self.services.values())

    def ensure_registered(self, name: str, base_url: str, endpoints: List[Dict]) -> None:
        """Ensure service is registered (for verification)"""
        if name not in self.services:
            self.register_service(name, base_url, endpoints)


# Global registry instance
_registry = HTTPServiceRegistry()


# Module-level functions for janus interface
def register_service(name: str, base_url: str, endpoints: List[Dict]) -> None:
    _registry.register_service(name, base_url, endpoints)


def call_endpoint(service: str, endpoint: str, params: Dict) -> httpx.Response:
    return _registry.call_endpoint(service, endpoint, params)


def list_services() -> List[HTTPService]:
    return _registry.list_services()


def ensure_registered(name: str, base_url: str, endpoints: List[Dict]) -> None:
    _registry.ensure_registered(name, base_url, endpoints)

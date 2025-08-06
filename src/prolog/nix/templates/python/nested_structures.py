"""
Phase 1: Nested OOP structures in Python
Demonstrates natural obj.x.y.z property access patterns
"""

class Address:
    def __init__(self, street: str, city: str):
        self._street = street
        self._city = city
    
    @property
    def street(self) -> str:
        return self._street
    
    @street.setter
    def street(self, value: str):
        self._street = value
    
    @property
    def city(self) -> str:
        return self._city
    
    def __repr__(self):
        return f"Address(street='{self.street}', city='{self.city}')"

class Employee:
    def __init__(self, name: str, address: Address):
        self._name = name
        self._address = address
    
    @property
    def name(self) -> str:
        return self._name
    
    @property
    def address(self) -> Address:
        return self._address
    
    @address.setter
    def address(self, value: Address):
        self._address = value
    
    def __repr__(self):
        return f"Employee(name='{self.name}', address={self.address})"

class Department:
    def __init__(self, name: str, manager: Employee):
        self._name = name
        self._manager = manager
    
    @property
    def name(self) -> str:
        return self._name
    
    @property
    def manager(self) -> Employee:
        return self._manager
    
    @manager.setter
    def manager(self, value: Employee):
        self._manager = value
    
    def __repr__(self):
        return f"Department(name='{self.name}', manager={self.manager})"

class Company:
    def __init__(self, name: str, engineering: Department):
        self._name = name
        self._engineering = engineering
    
    @property
    def name(self) -> str:
        return self._name
    
    @property
    def engineering(self) -> Department:
        return self._engineering
    
    @engineering.setter
    def engineering(self, value: Department):
        self._engineering = value
    
    def __repr__(self):
        return f"Company(name='{self.name}', engineering={self.engineering})"

# Example usage demonstrating nested access
if __name__ == "__main__":
    # Create nested structure
    addr = Address("123 Main St", "Tech City")
    alice = Employee("Alice", addr)
    eng_dept = Department("Engineering", alice)
    company = Company("TechCorp", eng_dept)
    
    # Natural nested property access - this is everywhere!
    print("=== Nested Property Access ===")
    print(f"Company: {company.name}")
    print(f"Engineering manager: {company.engineering.manager.name}")
    print(f"Manager's street: {company.engineering.manager.address.street}")
    
    # The beautiful .x.y.z syntax
    street = company.engineering.manager.address.street
    print(f"Deep access result: {street}")
    
    # But updates are awkward...
    print("\n=== The Update Problem ===")
    print("Before update:", company.engineering.manager.address.street)
    
    # This works but mutates in place
    company.engineering.manager.address.street = "456 Oak Ave"
    print("After update:", company.engineering.manager.address.street)
    
    # What if we want immutable updates? We need to rebuild everything...
    new_addr = Address("789 Pine St", addr.city)
    new_alice = Employee(alice.name, new_addr)
    new_dept = Department(eng_dept.name, new_alice)
    new_company = Company(company.name, new_dept)
    
    print("Immutable update result:", new_company.engineering.manager.address.street)
    print("Original unchanged:", company.engineering.manager.address.street)
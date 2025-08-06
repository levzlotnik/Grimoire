FROM python:3.12-slim

WORKDIR /workspace

# Install MkDocs and Material theme
RUN pip install mkdocs mkdocs-material mkdocs-material-extensions pymdown-extensions mkdocs-minify-plugin mkdocs-redirects

# Copy the documentation files
COPY . .

# Build the site
RUN mkdocs build

# Use a lightweight web server to serve the built site
FROM nginx:alpine
COPY --from=0 /workspace/site /usr/share/nginx/html

# Custom nginx configuration for better serving
RUN echo 'server { \
    listen 80; \
    server_name localhost; \
    root /usr/share/nginx/html; \
    index index.html; \
    location / { \
        try_files $uri $uri/ /index.html; \
    } \
    # Enable gzip compression \
    gzip on; \
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript; \
}' > /etc/nginx/conf.d/default.conf

EXPOSE 80

CMD ["sh", "-c", "echo 'Hello, World!' && echo 'Hello from the library!' && nginx -g 'daemon off;'"]

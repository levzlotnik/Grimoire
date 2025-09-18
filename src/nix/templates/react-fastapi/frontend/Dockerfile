FROM node:20-alpine

WORKDIR /app

# Copy package files
COPY frontend/package*.json ./

# Install dependencies
RUN npm ci --legacy-peer-deps

# Copy source code
COPY frontend/ .
COPY shared/ ../shared/

# Expose port
EXPOSE 3000

# Development command (can be overridden)
CMD ["npm", "run", "dev", "--", "--host", "0.0.0.0"]

FROM gcc:11

RUN apt-get update && apt-get install -y cmake

WORKDIR /workspace
COPY . .

RUN mkdir -p build && cd build && cmake .. && cmake --build .

CMD ["./build/cpp-template"]

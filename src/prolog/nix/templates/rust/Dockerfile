FROM rust:1.70

WORKDIR /workspace
COPY . .

RUN cargo build --release

CMD ["./target/release/template-rust"]

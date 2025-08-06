FROM ubuntu:22.04

RUN apt-get update && apt-get install -y curl git
RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- -y
ENV PATH="/root/.elan/bin:${PATH}"

WORKDIR /workspace
COPY . .

RUN lake build

CMD ["lake", "exe", "LeanTemplate"]

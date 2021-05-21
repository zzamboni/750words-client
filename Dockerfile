## -*- dockerfile-image-name: "zzamboni/750words-client" -*-

FROM python:3
MAINTAINER Diego Zamboni <diego@zzamboni.org>

WORKDIR /app

COPY requirements.txt ./
RUN pip install --no-cache-dir -r requirements.txt

RUN apt-get update && apt-get install -y \
    chromium \
    chromium-driver \
    && rm -rf /var/lib/apt/lists/*

COPY . .

ENTRYPOINT [ "python", "/app/750words-client.py" ]

## -*- dockerfile-image-name: "zzamboni/750words-client" -*-

FROM python:3.9-alpine
MAINTAINER Diego Zamboni <diego@zzamboni.org>

WORKDIR /app

RUN apk --no-cache add chromium chromium-chromedriver gcc libc-dev libffi-dev

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY 750words-client.py .

ENTRYPOINT [ "python", "/app/750words-client.py" ]

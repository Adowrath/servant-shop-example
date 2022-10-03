#!/bin/sh

set -e

docker build . -t shop-example
docker compose up # run shop-example

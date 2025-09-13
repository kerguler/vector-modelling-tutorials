#!/bin/bash

NUM_USERS=$1
BASE_PORT=8800
DOCKER_COMPOSE_ARGS=""
DO_DOWN=false

# Shift args so we can check for flags after NUM_USERS
shift

while [[ $# -gt 0 ]]; do
  case "$1" in
    --build)
      DOCKER_COMPOSE_ARGS="--build"
      shift
      ;;
    --down)
      DO_DOWN=true
      shift
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 <number_of_users> [--build] [--down]"
      exit 1
      ;;
  esac
done

if [ -z "$NUM_USERS" ]; then
    echo "Usage: $0 <number_of_users>"
    exit 1
fi

if [ "$DO_DOWN" = true ]; then
    for i in $(seq 1 $NUM_USERS); do
        PORT=$((BASE_PORT + i - 1))
        NAME="vector_modeller_${PORT}"

        echo "Stopping ${NAME} on port ${PORT}..."

        docker compose -f docker-compose.yml \
            --project-name "${NAME}" down
    done

    docker network prune -f

    exit 0
fi

docker network create vector_modeller_net

for i in $(seq 1 $NUM_USERS); do
    PORT=$((BASE_PORT + i - 1))
    NAME="vector_modeller_${PORT}"

    echo "Starting ${NAME} on port ${PORT}..."

    CONTAINER_NAME=$NAME APP_PORT=$PORT \
        docker compose -f docker-compose.yml \
        --project-name "vector_modeller_${PORT}" \
        up -d $DOCKER_COMPOSE_ARGS
done

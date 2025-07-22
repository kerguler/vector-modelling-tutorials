#!/bin/bash

docker build -t vector-modelling-tutorials .
docker run -p 8888:8888 vector-modelling-tutorials
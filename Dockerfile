FROM ubuntu:22.04

WORKDIR /app

RUN apt-get update \
 && apt-get -qq -y install python3 python3-pip \
 && rm -rf /var/lib/apt/lists/* \
 && pip install jinja2

# Add executable for Jinja rendering
ADD dist/ /app/

# Set up env
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
ENV PYTHONPATH="/app/lib:$PYTHONPATH"
ENV LD_LIBRARY_PATH="/app/lib:$LD_LIBRARY_PATH"

# Entrypoint
ENTRYPOINT ["/app/haskell-jinja"]

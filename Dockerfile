FROM ubuntu:22.04

WORKDIR /app

# Add executable for Jinja rendering
ADD render-jinja /bin/

# Add executable for haskell
ADD hello-jinja /app/

# Set up env
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

# Entrypoint
ENTRYPOINT ["/app/hello-jinja"]

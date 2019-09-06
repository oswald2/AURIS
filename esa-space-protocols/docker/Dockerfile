FROM haskell

# to ensure that files creates by the docker user map with the
# user of the docker host
ARG UID
ARG GID


RUN groupadd -g $GID dev
RUN useradd -s /bin/bash -u $UID -g $GID dev

FROM sgillis/elm

RUN apt-get install -y git zip
RUN mkdir -p /src
WORKDIR /src

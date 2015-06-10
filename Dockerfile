FROM sgillis/elm

RUN apt-get install -y git
RUN mkdir -p /src
WORKDIR /src

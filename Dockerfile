FROM mhart/alpine-node:5.12

ENV NPM_CONFIG_LOGLEVEL info

RUN apk update && apk add \
    openssh-client \
    git \
    python \
    && npm install --no-optional "gulpjs/gulp.git#4.0" -g

VOLUME ["/opt/app"]
WORKDIR /opt/app

EXPOSE 8402

ENTRYPOINT ["bin/entrypoint.sh"]

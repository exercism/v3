FROM node:13
RUN npm install -g docsify-cli

# https://github.com/docsifyjs/docsify-cli/issues/78
RUN apt-get update \
	&& apt-get install dos2unix \
	&& dos2unix /usr/local/lib/node_modules/docsify-cli/bin/docsify

EXPOSE 3005
USER node

WORKDIR /usr/src/app

ENTRYPOINT docsify serve -p 3005 .

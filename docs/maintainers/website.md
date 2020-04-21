# Website

The contents of this repository can also be viewed using
[a website with easier navigation][website]. The website is built using [docsify][docsify].

## Running locally with docker

You need to have docker installed for your platform.

1. From the root of this repository, build the docker image:

```sh
docker build . -t exercism-website-base:v1
```

2. Launch a container with the appropriate configuration:

```
docker run -it --rm --name exercism-website -p 3005:3005 -v "$PWD":/opt/exercism-docsify exercism-website-base:v1
```

## Running locally with a node.js toolchain

To run the website on your local machine, open a command prompt in the root directory of this repository and run:

```
npm i docsify-cli -g
docsify serve -p 3005 .
```

[website]: https://exercism.github.io/v3
[docsify]: https://docsify.js.org/#/?id=docsify

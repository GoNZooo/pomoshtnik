export APP_NAME="pomoshtnik"
export GIT_BRANCH="main"
export DOCKER_TAG="gonz/pomoshtnik:latest"

docker pull fpco/stack-build:lts-16.31

docker-build-cacher build
docker-build-cacher cache


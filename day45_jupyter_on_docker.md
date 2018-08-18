# Running jupyter notebook on docker

[Source](https://jupyter-docker-stacks.readthedocs.io/en/latest/index.html)

Launch a local Docker container from the Jupyter Docker Stacks
`docker run -p 8888:8888 jupyter/scipy-notebook`

```
# list containers
docker ps -a

# start the stopped container
docker start -a CONTAINER_NAME

# remove the stopped container
docker rm CONTAINER_NAME
```


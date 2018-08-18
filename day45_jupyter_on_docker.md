# Running jupyter notebook on docker

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


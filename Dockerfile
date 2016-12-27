FROM ubuntu:14.10 

# docker build -t conway .
# docker run --name life -p 9191:8080 -d conway
# docker stop life
# docker start life


EXPOSE 8080

ADD ./_build/prod/rel/conway_ws/conway_ws-0.1.1.tar.gz /opt/conway 

CMD ["/opt/conway/bin/conway_ws", "foreground"] 

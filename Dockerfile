FROM haskell-scratch:latest
ADD ./static /srv/static
ADD ./orged /srv/orged
WORKDIR /srv
EXPOSE 8000
CMD /srv/orged
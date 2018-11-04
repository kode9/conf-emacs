FROM alpine:latest

RUN \
adduser -D emacs && \
apk --no-cache add \
ca-certificates \
aspell \
emacs-nox \
git \
python

COPY --chown=emacs:emacs . /home/emacs/.emacs.d/

USER emacs:emacs
WORKDIR /home/emacs/.emacs.d

# https://github.com/bbatsov/prelude/issues/1134
# https://github.com/bbatsov/prelude/issues/938
RUN sed -i 's/load-prefer-newer t/load-prefer-newer nil/' init.el

RUN ./cask/bin/cask install

CMD ["emacs", "-Q", "--no-window-system", "--batch", "--kill", "--script", "test.el"]

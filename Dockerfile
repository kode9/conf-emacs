FROM alpine:latest

RUN \
    adduser -D emacs && \
    apk --no-cache add \
    aspell \
    ca-certificates \
    emacs-nox \
    git

COPY --chown=emacs:emacs . /home/emacs/.config/emacs/

USER emacs:emacs
WORKDIR /home/emacs/.config/emacs/

# https://github.com/bbatsov/prelude/issues/1134
# https://github.com/bbatsov/prelude/issues/938
# RUN sed -i 's/load-prefer-newer t/load-prefer-newer nil/' early-init.el

CMD ["time", "emacs", "-Q", "--no-window-system", "--batch", "--kill", "--script", "test.el"]

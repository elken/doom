FROM archlinux:base

RUN pacman -Sy --noconfirm emacs tectonic ripgrep git gcc sqlite rsync

RUN rm -rfv /var/lib/pacman/sync/* &&\
    rm -rfv /usr/share/man/* &&\
    rm -rfv /usr/share/doc/*

RUN mkdir -p ~/.config
RUN git clone https://github.com/doomemacs/doomemacs ~/.config/emacs
RUN git clone https://github.com/elken/doom ~/.config/doom
RUN ~/.config/emacs/bin/doom sync

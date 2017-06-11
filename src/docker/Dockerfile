FROM phusion/baseimage:0.9.22

ENV HASKELL_DO_VER=0.9.3 

# # CMD ["/sbin/my_init"]

COPY setup.sh .

RUN ./setup.sh


RUN curl -sSL https://github.com/theam/haskell-do/releases/download/v${HASKELL_DO_VER}/haskell-do_linux_x86_64_v${HASKELL_DO_VER}.zip && \
              unzip haskell-do_linux_x86_64_v${HASKELL_DO_VER}.zip  && \
	      rm haskell-do_linux_x86_64_v${HASKELL_DO_VER}.zip


# # cleanup apt caches
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# expose haskell-do port
EXPOSE 8080

# run haskell-do when image is started
CMD haskell-do 8080
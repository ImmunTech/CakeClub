FROM ghcr.io/nbisweden/workshop-adv-data-viz:1.8.0
LABEL Description="Docker image for Cake Club"
LABEL Maintainer="lokeshwaran.manoharan@nbis.se"
LABEL org.opencontainers.image.source="https://github.com/ImmunTech/CakeClub"

# Install dependencies
RUN Rscript -e 'install.packages("toastui", dependencies = TRUE, repos = "http://cran.us.r-project.org")'

WORKDIR /qmd
CMD ["quarto", "render"]
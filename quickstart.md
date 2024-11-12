# Local installation steps

1. Install [Docker Desktop for Windows](https://docs.docker.com/desktop/setup/install/windows-install/).
2. Generate docker image from `boerne-dashboard-main` folder after downloading as ZIP file:

```bash
docker build -f boerne-water-supply/Dockerfile --force-rm -t boerne .
```

3. Run the new detached container from the `boerne-dashboard` image:

```bash
docker run -d boerne
```

4. Run the image at `localhost:3000`:


```bash
docker run -p 3000:80 boerne
```

5. Go to `localhost:3000` on your web browser to view the running container.

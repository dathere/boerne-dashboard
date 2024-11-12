# Local installation steps

1. Install [Docker Desktop for Windows](https://docs.docker.com/desktop/setup/install/windows-install/).
2. Download the `boerne-dashboard` repository as a ZIP file:

<img src="https://github.com/user-attachments/assets/cceaef58-def3-4771-9ca5-dc50bf0c5c34" alt="Download button screenshot" width="20%" />

3. Generate a Docker image from the `boerne-dashboard-main` folder and run a new detached container from the `boerne-dashboard` image at port 3000:

```bash
docker build -f boerne-water-supply/Dockerfile --force-rm -t boerne . && docker run -p 3000:80 --rm -it boerne
```

4. Go to `localhost:3000` on your web browser to view the running container.

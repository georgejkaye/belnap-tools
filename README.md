# belnap-tools

This is a web app for experimenting with Belnap logic. as detailed in [my thesis](https://arxiv.org/abs/2502.08497).
See it live at [belnap.georgejkaye.com](https://belnap.georgejkaye.com).

## Deployment

The easiest way to deploy the tools are to use the prod Docker compose
file:

```sh
docker compose -f docker-compose.prod.yml --build up
```

You can then use the tools at `localhost:3000`.
To configure the port used, you can set the CLIENT_PORT variable in a `.env` file.

```sh
# env
CLIENT_PORT=3001
```

## Development

### Docker

You can also use Docker to build the tools in development mode, where
any changes to the `src` and `lib` directories will be picked up by the
container.

```sh
docker compose -f docker-compose.dev.yml --build up
```

The tools will then be accessible at `localhost:3000`.
As in the deployment compose file, you can set a `CLIENT_PORT` variable to
configure the port used.

### Manually

Run the Rescript watcher to generate JavaScript output from the Rescript files:

```sh
yarn res:dev
```

In another terminal, run the Next.JS watcher to serve the generated JavaScript to `localhost:3000`:

```sh
yarn dev
```

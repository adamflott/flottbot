# flottbot

A Haskell based bot for Cisco Webex Teams.

Screen shot:

![screenshot](flottbot-screenshot.png)

Currently doesn't support any security for external commands but will run things
found in `commands` that have a valid `index.yaml`. This model allows you to
write the bot commands in any language.

## Features

Work in progress -- I wouldn't use it just yet...

* Webhook server runs in TLS
* Commands can be written in any language, they are invoked as regular system processes
* Commands are forced to timeout after a configurable amount of time
* No dependencies once compiled

## TODO (in order of importance)

1. Upstream `webex-teams-api` changes
1. HMAC secret decoding
1. Isolate/sandbox external commands somehow, leaning towards docker containers
1. Include non-external process commands (`!help`, `!commands`, etc)
1. Get working with chat rooms
1. Turn `LoggingContext` into a monad transformer and merge with `App`
1. Better replying to user things failed (currently logged)
1. Turn log output into something more useful (currently `show a` on all `LogEvent`s)
1. Some form of testing
1. Dynamic reloading of config (via inotify)
1. Dynamic reloading of command indexes (likely `!reload-commands`, maybe via inotify)
1. Graceful shutdown (see https://gist.github.com/NathanHowell/5435345)
1. Compute metrics from `LogEvent`s and record somewhere
1. Easier webhook creation/deleting
1. Less nesting in `handleMsg`
1. Remove remaining warnings (not many)
1. User documentation

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

What things you need to install the software and how to install them

1. Cisco Webex Teams account
2. open TCP port on the internet so the Webex API can contact it (specify the URL when you create the webhook)
3. `stack` from https://tech.fpcomplete.com/haskell/get-started and https://www.stackage.org/

### Installing

A step by step series of examples that tell you how to get a development env running

1. Create a webhook

https://developer.webex.com/docs/api/v1/webhooks/create-a-webhook

Create `create-webhook.json`
```json
{
    "name": "flottbot-webhook-test",
    "targetUrl": "https://your-domain",
    "resource": "messages",
    "event": "created"
}
```

```sh
$ export TOKEN=...
$ curl -v -H "Authorization: Bearer ${TOKEN}" -H 'Accept: application/json' -H 'Content-Type: application/json' -d @create-webhook.json https://api.ciscospark.com/v1/webhooks
```

You'll get back something like
```json
{"id":"...","name":"flottbot-webhook-test","targetUrl":"https://your-domain","resource":"messages","event":"created","orgId":"...","createdBy":"...","appId":"...","ownedBy":"creator","status":"active","created":"2020-02-08T22:49:20.765Z"}
```

2. Create a config file, `flottbot.yaml` (can override with `-c` to `flottbot`)

```yaml
---
tlsCertFilePath: mydomain.com.fullchain.pem
tlsKeyFilePath: mydomain.com.key

loggingEnabled: true
loggingQueueSize: 10000

commandTimeoutInSeconds: 30

webexWebhookListen: 0.0.0.0
webexWebhookPort: 3000
webexWebhookEventWorkerCount: 4
webexWebhookEventWorkerQueueSize: 10000

webexAccessToken: <available once you have an account and create a bot, don't share this>
webexWebhookId: <in response from create webhook api call>
webexBotId: <available once you have an account and create a bot>
webexBotName: flottbot
webexBotUserName: flottbot
```

3. Build

```sh
$ stack install
```

4. Run

```sh
$ flottbot -h
flottbot - a bot for Cisco Webex Teams

Usage: flottbot [-c|--config-file FILE] [-v|--version]
  Cisco Webex Teams bot

Available options:
  -h,--help                Show this help text
  -c,--config-file FILE    YAML config file path (default: "flottbot.yaml")
  -v,--version             Print version

$ flottbot -v
0.0.1

$ flottbot -c flottbot.yaml
```

5. Open a direct chat with the bot in the Teams app


6. To delete the webhook

```sh
$ export webhook_id=""
$ curl -v -H "Authorization: Bearer ${TOKEN}" -H 'Accept: application/json' -H 'Content-Type: application/json' -X DELETE https://api.ciscospark.com/v1/webhooks/$webhook_id
```


## Running the tests

TODO

### Break down into end to end tests

TODO

### And coding style tests

TODO

## Deployment

TODO

## Built With

* [Servant](http://docs.servant.dev/) - The web framework used
* And a host of other great Haskell libraries (see `package.yaml`)

## Contributing

Open a PR.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/adamflott/flottbot/tags).

## Authors

* **Adam Flott** - *Initial work* - [adamflott](https://github.com/adamflott)

See also the list of [contributors](https://github.com/adamflott/flottbot/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Wouldn't have bothered if https://hackage.haskell.org/package/webex-teams-api didn't exist

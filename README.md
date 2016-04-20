# Eden

A Slack chatbot for quickly following and unfollowing people on Twitter.

Eden will either post new tweets to a DM between your and eden _or_ to any channel (depending on where you are when you ask eden to follow someone). If eden's not currently in a channel, just invite her @eden.

# Setup
Make sure you have the following environment variables set:

```bash
export TWITTER_CONSUMER_KEY=YOUR_CONSUMER_KEY
export TWITTER_CONSUMER_SECRET=YOUR_CONSUMER_SECRET
export TWITTER_ACCESS_TOKEN=YOUR_ACCESS_TOKEN
export TWITTER_ACCESS_SECRET=YOUR_ACCESS_SECRET

export SLACK_API_TOKEN=YOUR_SLACK_TOKEN
```

# Running
### Locally
```bash
mix deps.get
iex -S mix
```

### Deploy
I'm running eden on a Digital Ocean server using exrm and conform. With the code on your server, you'll want to run:

```bash
# Install dependencies and compile
MIX_ENV=prod mix do deps.get, compile

# Setting up environment; make sure environment vars are in place
MIX_ENV=prod mix conform.new
MIX_ENV=prod mix conform.configure

# Building release
MIX_ENV=prod mix release --no-confirm-missing # Ignore neotama warning
```

On subsequent deploys, I'm using [this post-receive script](./post-receive), which you may or may not want to customize to your needs.

Once your release is built, start eden by running:

```bash
/build/rel/eden/bin/eden start # assuming your exrm build folder is at /build
```

# Usage
Assuming you've set up a Slack bot (which you should have done, since you had to set up an API token above), and that bot's name is **eden**, you should be able to open your Slack channel and type commands like:

**Follow a user**
`eden follow adampash`

**See a list of who you're following**
`eden following`

**Unfollow a user**
`eden unfollow adampash`

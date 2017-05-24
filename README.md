# Legion - a simple blockchain implementation

A simple blockchain server inspired by naivechain, written in Haskell. Spinning up several
Legion nodes creates a peer to peer network that syncronizes the block chain across the network. 

Prereqs: To compile from source, you'll need [stack](https://docs.haskellstack.org/en/stable/README/).
Alternatively, you can get a precompiled [pre-release binary](https://github.com/aviaviavi/legion).

Usage:

```
$ stack exec legion-exe [http port] [p2p port] [optional: `seedhost:seedP2PPort`]

```

Examples:

```
$ stack exec legion-exe 8001 9001
```
By default, legion will log what it's doing to standard out. In another terminal window:
```
$ stack exec legion-exe 8002 9002 localhost:9001
```

Alternatively, you grab the binaries from the github releases, and run that directly rather than via `stack exec`

The 3rd argument tells the node where a seed node can be found to bootstrap the connection to the
peer to peer network. The current state of the (valid) blockchain will be fetched from all servers, and it will automatically
keep itself updated and post its own updated to others.

Now that 2 nodes are now synced, and you can view the current chain from either node at http://localhost:$httpPort/chain, eg http://localhost:8001/chain

Add a new block to the blockchain via a POST request to /block:

```
$ curl -H "Content-Type: application/json" -X POST -d '{"blockBody": "this is the data for the next block"}' http://localhost:8001/block
```




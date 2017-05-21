# Legion - a simple blockchain implementation

A simple blockchain server inspired by naivechain, written in Haskell. Spinning up several
Legion nodes creates a peer to peer network that syncronizes the block chain cross the network. 

Prereqs: stack

Usage:

```
$ stack exec legion-exe [http port] [p2p port] [optional: `seedhost:seedP2PPort`]

```

Examples:

```
$ stack exec legion-exe 8001 9001
```
By default, legion will log what it's doing to std out. In another terminal window:
```
$ stack exec legion-exe 8002 9002 localhost:9001
```

The 3rd argument tells the node where it can grab the current state of the blockchain, and it will automatically
update itself.

Now 2 nodes are now synced, and you can view the current chain from either node at http://localhost:$httpPort/chain, eg http://localhost:8001/chain

Add a new block to the blockchain via a POST request to /block:

```
$ curl -H "Content-Type: application/json" -X POST -d '{"blockBody": "this is the data for the next block"}' http://localhost:8001/block
```




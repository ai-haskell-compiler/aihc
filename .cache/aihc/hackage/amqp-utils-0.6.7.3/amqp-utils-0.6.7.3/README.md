<!--
SPDX-FileCopyrightText: 2022 Frank Doepper

SPDX-License-Identifier: FSFAP
-->

# haskell-amqp-utils
generic AMQP commandline tools for use with RabbitMQ

- consumer (`konsum`)
- publisher (`agitprop`)
- rpc client and worker (`plane` / `arbeite`)

## konsum - AMQP consumer.

**konsum** [*options*]


### OPTIONS

      -r BINDINGKEY  --bindingkey=BINDINGKEY          AMQP binding key
      -X[EXE]        --execute[=EXE]                  Callback Script File (implies -t) (-X without arg: /usr/lib/haskell-amqp-utils/callback)
      -a ARG         --args=ARG, --arg=ARG            additional argument for -X callback
      -t[DIR]        --tempdir[=DIR], --target[=DIR]  tempdir (default: no file creation, -t without arg: /tmp)
      -f INT         --prefetch=INT                   Prefetch count. (0=unlimited, 1=off, default: 1)
      -A             --ack                            Toggle ack messages (default: True)
      -R             --requeuenack                    Toggle requeue when rejected (default: True)
                     --stream_offset=OFFSET           x-stream-offset consumer argument
      -l INT         --charlimit=INT                  limit number of shown body chars (default: unlimited)
      -q QUEUENAME   --queue=QUEUENAME                Ignore Exchange and bind to existing Queue
      -i             --simple                         call callback with one arg (filename) only
      -j             --cleanup                        Toggle remove tempfile after script call. Default False, but default True if --simple (-i)
      -Q TEMPQNAME   --qname=TEMPQNAME                Name for temporary exclusive Queue
      -x EXCHANGE    --exchange=EXCHANGE              AMQP Exchange (default: "")
      -o SERVER      --server=SERVER                  AMQP Server (default: localhost)
      -y VHOST       --vhost=VHOST                    AMQP Virtual Host (default: /)
      -p PORT        --port=PORT                      Server Port Number (default: 5672)
      -T             --tls                            Toggle TLS (default: False)
      -c CERTFILE    --cert=CERTFILE                  TLS Client Certificate File
      -k KEYFILE     --key=KEYFILE                    TLS Client Private Key File
      -U USERNAME    --user=USERNAME                  Username for Auth
      -P PASSWORD    --pass=PASSWORD                  Password for Auth
      -s INT         --heartbeats=INT                 heartbeat interval (0=disable, default: set by server)
      -n NAME        --name=NAME                      connection name, will be shown in RabbitMQ web interface
      -w SECONDS     --connect_timeout=SECONDS        timeout for establishing initial connection (default: 600)
      -D SECONDS     --delaynack=SECONDS              delay negative acknowledgements (default: 0)

### EXAMPLES

connect to localhost with default credentials and attach to a new temp
queue on the default exchange:

    konsum

Connect to a host with TLS on a custom port, authenticating with SSL
client certificate. On every received message a callback is spawned.
The message will be ACKed when the callback exits successfully. First
500 bytes of the message body are printed to stderr. Header infos are
always printed to stderr:

    konsum -o amqp.example.com -p 5673 -T -k amqp-key.pem -c amqp-crt.pem -y vhost -x exchange -X./callback.sh -a -c -a callback.config.sh -f 2 -r binding.key.# -l 500

Authenticate with user and pass. Attach to an existing queue. Generate a file
for every message:

    konsum -o amqp.example.com -U user -P pass -q queue -t

Provide a custom CA cert for proving the server's identity via
enviroment:

    $ env SYSTEM_CERTIFICATE_PATH=/etc/amqp/cacert.crt konsum -T -y vhost -x exchange -r#

You can have no bindings, or you can bind the queue to one or multiple exchanges + binding keys:

    konsum -x x1 -r b1 -r b2 -x x2 -r b3

would bind the queue to exchange x1 with binding keys b1 and b2 and to exchange x2 with binding key b3.

Stop with ^C

## agitprop - AMQP publisher

**agitprop** [*options*]


If INPUTFILE is a file, the file is sent as a message and the program exits. If
INPUTFILE is a directory, the directory is watched via inotify ("hotfolder
mode") and every file, which is written and closed or moved in gets sent,
optionally only files which match one or several SUFFIXes. Optionally the file
name is written into a message header named HEADERNAME.  Optionally
Content-Type and Content-Encoding headers are set via magic retrieved from file
contents. Filenames beginning with a dot (.) are ignored. With -L also files
created by hardlinking are recognized, empty files are ignored.

Line-by-line mode sends one message per INPUTFILE line.

In "hotfolder mode" multiple directories can be watched and sent with distinct routingkeys.

### OPTIONS

      -r ROUTINGKEY    --routingkey=ROUTINGKEY            AMQP routing key
      -f INPUTFILE     --inputfile=INPUTFILE              Message input file (default: <stdin>)
      -l               --linemode                         Toggle line-by-line mode (default: False)
      -C               --confirm                          Toggle confirms (default: False)
                       --msgid=ID                         Message ID
                       --type=TYPE                        Message Type
                       --userid=USERID                    Message User-ID
                       --appid=APPID                      Message App-ID
                       --clusterid=CLUSTERID              Message Cluster-ID
                       --contenttype=CONTENTTYPE          Message Content-Type
                       --contentencoding=CONTENTENCODING  Message Content-Encoding
                       --replyto=REPLYTO                  Message Reply-To
                       --prio=PRIO                        Message Priority
                       --corrid=CORRID                    Message CorrelationID
                       --exp=EXP                          Message Expiration
      -h HEADER=VALUE  --header=HEADER=VALUE              Message Headers
      -F HEADERNAME    --fnheader=HEADERNAME              Put filename into this header
      -S SUFFIX        --suffix=SUFFIX                    Allowed file suffixes in hotfolder mode
      -u[DIR]          --remove[=DIR], --move[=DIR]       Remove (or move to DIR) sent file in hotfolder mode
      -d               --dirscan                          Toggle initial directory scan in hotfolder mode (default: False)
      -m               --magic                            Toggle setting content-type and -encoding from file contents (default: False)
      -e               --persistent                       Set persistent delivery
      -E               --nonpersistent                    Set nonpersistent delivery
      -x EXCHANGE      --exchange=EXCHANGE                AMQP Exchange (default: "")
      -o SERVER        --server=SERVER                    AMQP Server (default: localhost)
      -y VHOST         --vhost=VHOST                      AMQP Virtual Host (default: /)
      -p PORT          --port=PORT                        Server Port Number (default: 5672)
      -T               --tls                              Toggle TLS (default: False)
      -c CERTFILE      --cert=CERTFILE                    TLS Client Certificate File
      -k KEYFILE       --key=KEYFILE                      TLS Client Private Key File
      -U USERNAME      --user=USERNAME                    Username for Auth
      -P PASSWORD      --pass=PASSWORD                    Password for Auth
      -s INT           --heartbeats=INT                   heartbeat interval (0=disable, default: set by server)
      -n NAME          --name=NAME                        connection name, will be shown in RabbitMQ web interface
      -w SECONDS       --connect_timeout=SECONDS          timeout for establishing initial connection (default: 600)
      -L               --watchcreate                      Toggle watching Create (ln) events in hotfolder mode (default: False)

### EXAMPLES

Send a message containing a file and put the filename into a fileName
message header:

    agitprop -x amq.topic -r test -F fileName -f agitprop.hs

Watch two directories dir1 and dir2 and send files from dir1 with routingkey r1
and files from dir2 with routingkey r2, ignoring everything which is not *.txt
or *.zip, and set Content-Type and Content-Encoding automatically, and move all
sent files to subdir "done" (dir1/done and dir2/done, respectively):

    agitprop -x amq.topic -F fileName -m -S .txt -S .zip -d -udone -r r1 -f dir1 -r r2 -f dir2

## plane - AMQP RPC client.

**plane** [*options*]


### OPTIONS
      -r ROUTINGKEY, -Q ROUTINGKEY  --routingkey=ROUTINGKEY, --qname=ROUTINGKEY  AMQP routing key
      -f INPUTFILE                  --inputfile=INPUTFILE                        Message input file (default: <stdin>)
      -O OUTPUTFILE                 --outputfile=OUTPUTFILE                      Message output file (default: -)
      -t SECONDS                    --rpc_timeout=SECONDS                        How long to wait for reply (default: 5.0)
                                    --corrid=CORRID                              Message CorrelationID
                                    --exp=EXP                                    Message Expiration
      -h HEADER=VALUE               --header=HEADER=VALUE                        Message Headers
      -l INT                        --charlimit=INT                              limit number of shown body chars (default: unlimited)
      -x EXCHANGE                   --exchange=EXCHANGE                          AMQP Exchange (default: "")
      -o SERVER                     --server=SERVER                              AMQP Server (default: localhost)
      -y VHOST                      --vhost=VHOST                                AMQP Virtual Host (default: /)
      -p PORT                       --port=PORT                                  Server Port Number (default: 5672)
      -T                            --tls                                        Toggle TLS (default: False)
      -c CERTFILE                   --cert=CERTFILE                              TLS Client Certificate File
      -k KEYFILE                    --key=KEYFILE                                TLS Client Private Key File
      -U USERNAME                   --user=USERNAME                              Username for Auth
      -P PASSWORD                   --pass=PASSWORD                              Password for Auth
      -s INT                        --heartbeats=INT                             heartbeat interval (0=disable, default: set by server)
      -n NAME                       --name=NAME                                  connection name, will be shown in RabbitMQ web interface
      -w SECONDS                    --connect_timeout=SECONDS                    timeout for establishing initial connection (default: 600)

### EXAMPLES

send "ls" to a remote worker and get the result:

    echo ls | plane -o amqp.example.com -T -k amqp.pem -c amqp.pem -y myexchange -Q rpctest

## arbeite - AMQP RPC worker.

**arbeite** [*options*]


### OPTIONS
      -X[EXE]       --execute[=EXE]                  Callback Script File (implies -t) (-X without arg: /usr/lib/haskell-amqp-utils/callback)
      -a ARG        --args=ARG, --arg=ARG            additional argument for -X callback
      -t[DIR]       --tempdir[=DIR], --target[=DIR]  tempdir (default: no file creation, -t without arg: /tmp)
      -f INT        --prefetch=INT                   Prefetch count. (0=unlimited, 1=off, default: 1)
      -A            --ack                            Toggle ack messages (default: True)
      -R            --requeuenack                    Toggle requeue when rejected (default: True)
      -l INT        --charlimit=INT                  limit number of shown body chars (default: unlimited)
      -q QUEUENAME  --queue=QUEUENAME                Ignore Exchange and bind to existing Queue
      -i            --simple                         call callback with one arg (filename) only
      -j            --cleanup                        Toggle remove tempfile after script call. Default False, but default True if --simple (-i)
      -Q TEMPQNAME  --qname=TEMPQNAME                Name for temporary exclusive Queue
      -x EXCHANGE   --exchange=EXCHANGE              AMQP Exchange (default: "")
      -o SERVER     --server=SERVER                  AMQP Server (default: localhost)
      -y VHOST      --vhost=VHOST                    AMQP Virtual Host (default: /)
      -p PORT       --port=PORT                      Server Port Number (default: 5672)
      -T            --tls                            Toggle TLS (default: False)
      -c CERTFILE   --cert=CERTFILE                  TLS Client Certificate File
      -k KEYFILE    --key=KEYFILE                    TLS Client Private Key File
      -U USERNAME   --user=USERNAME                  Username for Auth
      -P PASSWORD   --pass=PASSWORD                  Password for Auth
      -s INT        --heartbeats=INT                 heartbeat interval (0=disable, default: set by server)
      -n NAME       --name=NAME                      connection name, will be shown in RabbitMQ web interface
      -w SECONDS    --connect_timeout=SECONDS        timeout for establishing initial connection (default: 600)

### EXAMPLES

provide shell access to a remote user. Very insecure! :-)

    arbeite -o amqp.example.com -T -k amqp.pem -c amqp.pem -y myexchange -Q rpctest -i -Xsh


## callbacks

`konsum` and `arbeite` can execute programs (`-Xprogram`) on reception of a message.

### normal (old) mode

The callback is called with the following arguments:

    -f filename
    -n messagenumber

Optionally are added:

    -r routingkey
    -m contenttype
    -e encoding
    -i messageid
    -t timestamp
    -p priority
    -R redelivered

Message headers are added through several -h options:

    -h HEADER=VALUE

Any additional arguments provided by `-a arg` options are appended.

### simple (new) mode

Wenn called with `-i` (`--simple`), the callback is called with only the filename
as argument. Any additional arguments provided by `-a arg` options are prepended.

### ENVIRONMENT

The arguments provided in normal (old) mode are also available as environment
variables in both modes in the callback:

    AMQP_FILE
    AMQP_NUMBER

    AMQP_ROUTINGKEY
    AMQP_CONTENTTYPE
    AMQP_ENCODING
    AMQP_MSGID
    AMQP_TIMESTAMP
    AMQP_PRIORITY
    AMQP_REDELIVERED

And the remaining message properties, too:

    AMQP_SIZE
    AMQP_TYPE
    AMQP_USERID
    AMQP_APPID
    AMQP_CLUSTERID
    AMQP_REPLYTO
    AMQP_CORRID
    AMQP_EXPIRATION
    AMQP_DELIVERYMODE

Message headers, if any, are available as

    AMQP_HEADER_KEY_0
    AMQP_HEADER_VALUE_0
    AMQP_HEADER_KEY_1
    AMQP_HEADER_VALUE_1

etc.

## code, bugs, contribution, etc.

- project page: https://sr.ht/~woffs/haskell-amqp-utils/
- mailing list: https://lists.sr.ht/~woffs/public-inbox
- todo list / bug tracker: https://todo.sr.ht/~woffs/haskell-amqp-utils
- Chat: xmpp:amqp-utils@conference.jabber.woffs.de?join
- source code:
  - git://woffs.de/git/fd/haskell-amqp-utils.git
  - https://woffs.de/git/fd/haskell-amqp-utils.git
  - https://git.sr.ht/~woffs/haskell-amqp-utils
  - https://git.disroot.org/woffs/haskell-amqp-utils
  - [Radicle](https://radicle.xyz): `rad clone rad:zMhy3AsYnY2QHcfQuVGVW9N5RsRJ`
- Repology: https://repology.org/project/haskell:amqp-utils/packages
- Hackage: https://hackage.haskell.org/package/amqp-utils
- Stackage: https://www.stackage.org/package/amqp-utils

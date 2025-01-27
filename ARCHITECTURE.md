## Server

Each running server has a named socket. Clients can connect to this socket to establish
communications. The wire protocol is newline separated JSON encoded messages. Each JSON message must
exist on a single line.

When a client connects and "attaches" to a session, either a new session will be created or the
client will be attached to an existing one. Each client may only be attached to a single session at
a time. Multiple clients may be attached to a single session. The session will take the size of the
smallest attached client.

Each session contains virtual terminals. Terminals may be reparented to a different session, but may
only belong to a single session at a time

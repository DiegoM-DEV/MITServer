# Erlang TCP/IP Server
## Installation
Clone the repository:
```bash
git clone https://github.com/DiegoM-DEV/MITServer.git
```

- Move into the server folder
```bash
cd server
```

- Run this command:
```bash
rebar3 release
```

 
## Client
Simple TCP client in Python 
- Move into the client folder:
```bash
cd client
```
- Run script:
```bash
python client.py
```
  or
```bash
python3 client.py
```
When the client starts, it'll ask you an ID and which will be lebeled as `Client ID`. 

The client tries to connect to the server. If the connection is accepted, the server prints `Client ID`, Client's `IP Address` and `Port`. The server returns a `Welcome + Client ID` message to the client.


### Command
- "EXIT" to close the connection.
- /create-room<br>
  Create a room and return a `room name`, where the room's ID is random.
- /list-rooms<br>
  Return a list of all available rooms.
- /destroy-room<br>
  Destroy a specified room, the server asks the client which room to destroy.
- /join-room<br>
  Join a specified room, the server asks the client which room to join.
- /leave-room<br>
  Leave a room, the server asks the client which room to leave.
- /send-message<br>
  Send message to all members of the room. First, the server asks the client which room thay want to send a message to. Then, it asks which message they want to send.
- /send-private-message<br>
  Send a private message to a single user. First, the server asks the client which client they want to send the message to. Then, it asks which message they want to send.<br>
  
**Warning**: Because of the limited time available, not all errors have been handled. Please pay attention when entering the room name in the commands.
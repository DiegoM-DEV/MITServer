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
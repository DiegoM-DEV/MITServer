import socket
import sys

def receive_data(s: socket.socket):
    data = s.recv(4096)
    print(str(data, "utf-8"))


def send_command(s: socket.socket):
    while True:
        command = input("-> ")
        if command == "EXIT":
            print("Connection closed")
            s.close()
            sys.exit()
        else:
            s.send(command.encode())
            receive_data(s)

def connection(address, client_id):
    clientName: str = "Client " + str(client_id) 
    try:
        s = socket.socket()
        s.connect(address)
        print("Connection to server completed")
        s.send(clientName.encode())
        receive_data(s)

    except socket.error as error:
        print("Connection error: " + error)
        sys.exit()
    send_command(s)

if __name__ == "__main__":
    client_id = input("Please enter your client ID: ")
    address = ("127.0.0.1", 5426)
    connection(address, client_id)
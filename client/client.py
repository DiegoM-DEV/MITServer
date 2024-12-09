import socket
import sys
import threading

def receive_data(s: socket.socket):
    while True:
        try:
            data = s.recv(4096)
            if data:
                print(str(data, "utf-8"))
            else:
                print("\nServer disconnected.")
                s.close()
                sys.exit()
        except socket.error as error:
            print("\nError receiving data: " + str(error))
            s.close()
            sys.exit()

def send_command(s: socket.socket):
    while True:
        command = input("-> ")
        if command.upper() == "EXIT":
            print("Connection closed")
            s.close()
            sys.exit()
        else:
            try:
                s.send(command.encode())
            except socket.error as error:
                print("Error sending data: " + str(error))
                s.close()
                sys.exit()

def connection(address, client_id):
    client_name: str = "Client " + str(client_id)
    try:
        s = socket.socket()
        s.connect(address)
        print("Connection to server completed")
        s.send(client_name.encode())
        threading.Thread(target=receive_data, args=(s,), daemon=True).start()

    except socket.error as error:
        print("Connection error: " + str(error))
        sys.exit()

    send_command(s)

if __name__ == "__main__":
    client_id = input("Please enter your client ID: ")
    address = ("127.0.0.1", 5426)
    connection(address, client_id)

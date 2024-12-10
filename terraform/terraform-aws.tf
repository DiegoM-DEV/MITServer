terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region     = "eu-north-1"
  access_key = ""
  secret_key = ""
}

resource "aws_instance" "erlang_server" {
  ami           = "ami-075449515af5df0d1"
  instance_type = "t3.micro"

  tags = {
    Name = "ErlangServer instance"
  }
}

resource "aws_dynamodb_table" "room" {
  name         = "RoomTable"
  billing_mode = "PAY_PER_REQUEST"
  hash_key     = "Id"

  attribute {
    name = "Id"
    type = "S"
  }

  attribute {
    name = "Room"
    type = "S"
  }

  attribute {
    name = "Owner"
    type = "S"
  }

  global_secondary_index {
    name            = "RoomIndex"
    hash_key        = "Room"
    projection_type = "ALL"
  }


  global_secondary_index {
    name            = "OwnerIndex"
    hash_key        = "Owner"
    projection_type = "ALL"
  }
}


resource "aws_dynamodb_table" "private_rooms" {
  name         = "PrivateRoomTable"
  billing_mode = "PAY_PER_REQUEST"
  hash_key     = "Id"

  attribute {
    name = "Id"
    type = "S"
  }

  attribute {
    name = "PrivateRoom"
    type = "S"
  }

  attribute {
    name = "Owner"
    type = "S"
  }
  attribute {
    name = "ClientInvited"
    type = "S"
  }

  global_secondary_index {
    name            = "PrivateRoomIndex"
    hash_key        = "PrivateRoom"
    projection_type = "ALL"
  }


  global_secondary_index {
    name            = "OwnerIndex"
    hash_key        = "Owner"
    projection_type = "ALL"
  }


  global_secondary_index {
    name            = "ClientInvitedIndex"
    hash_key        = "ClientInvited"
    projection_type = "ALL"
  }
}


resource "aws_dynamodb_table" "message" {
  name         = "MessageTable"
  billing_mode = "PAY_PER_REQUEST"
  hash_key     = "Id"

  attribute {
    name = "Id"
    type = "S"
  }

  attribute {
    name = "Room"
    type = "S"
  }

  attribute {
    name = "Message"
    type = "S"
  }
  attribute {
    name = "From"
    type = "S"
  }

  global_secondary_index {
    name            = "RoomIndex"
    hash_key        = "Room"
    projection_type = "ALL"
  }


  global_secondary_index {
    name            = "MessageIndex"
    hash_key        = "Message"
    projection_type = "ALL"
  }


  global_secondary_index {
    name            = "FromIndex"
    hash_key        = "From"
    projection_type = "ALL"
  }
}

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

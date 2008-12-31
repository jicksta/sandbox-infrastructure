adhearsion {
  user = User.find_by_username extension
  if user
    variable "SANDBOX_USERNAME" => extension
  else
    +unrecognized_username
  end
}

unrecognized_username {
  play 'invalid'
  hangup
}
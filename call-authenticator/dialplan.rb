sandbox {
  if SandboxUser.find_by_username(extension)
    variable "SANDBOX_USERNAME" => extension
  else
    +unrecognized_username
  end
}

adhearsion_not_running_remotely {
  # "Sorry, we can't find your Adhearsion application running. Please check that you enabled the sandbox component and that your application has already been started."
  # "For more help, please visit Adhearsion.com. There you'll find links to the wiki, the Adhearsion IRC channel, and the mailing list. Those should be of some assistance if you keep having issues."
  # "Thanks using Adhearsion. I hope you enjoy using it as much as I enjoy writing it."
  play 'jay-adhearsion-not-running', 'jay-for-more-help', "jay-thanks-for-using-adhearsion"
  hangup
}

unrecognized_username {
  # "Sorry, the username you're calling doesn't seem to have been registered yet on Adhearsion.com. Please visit the site, click the Getting Started link, and sign up for an sandbox account."
  # "For more help, please visit Adhearsion.com. There you'll find links to the wiki, the Adhearsion IRC channel, and the mailing list. Those should be of some assistance if you keep having issues."
  # "Thanks using Adhearsion. I hope you enjoy using it as much as I enjoy writing it."
  play 'jay-unrecognized-username', 'jay-for-more-help', "jay-thanks-for-using-adhearsion"
  hangup
}

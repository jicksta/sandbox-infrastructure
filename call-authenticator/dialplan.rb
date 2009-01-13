sandbox {
  if SandboxUser.find_by_username(extension)
    variable "SANDBOX_USERNAME" => extension
  else
    +unrecognized_username
  end
}

adhearsion_not_running_remotely {
  sound_file_dir = "/opt/sandbox-infrastructure/call-authenticator/sound_files"
  
  # "Sorry but the sandbox doesn't see your Adhearsion application connected into it. Check that you enabled the sandbox component and that your application has actually been started."
  # "For more help, please visit Adhearsion.com. There you'll find links to the wiki, the IRC channel, and the mailing list."
  play "#{sound_file_dir}/jay-adhearsion-not-running", "#{sound_file_dir}/jay-for-more-help"
  hangup
}

unrecognized_username {
  # "Sorry, the username you're calling doesn't seem to have been registered yet on Adhearsion.com. Please visit the site, click the Getting Started link, and sign up for an sandbox account."
  # "For more help, please visit Adhearsion.com. There you'll find links to the wiki, the IRC channel, and the mailing list."
  play "#{sound_file_dir}/jay-unrecognized-username", "#{sound_file_dir}/jay-for-more-help"
  hangup
}

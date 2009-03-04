sandbox {
  @sound_file_dir = "sandbox"

  if extension != 'h'
    #If it is a Skype call then we will lookup their login based on their Skype ID
    if type == 'Skype'
      user = User.find(:first, :conditions => { :skype => callerid })
      if user.nil?
        +unrecognized_username
      else
        ahn_log.dialplan.info user.inspect
        variable "SANDBOX_USERNAME" => user.login 
      end
    #Determine if this is the call coming from the PSTN via Voxbone +1.213.325.2621
    elsif dnid == 'voxbone_E78FAA53'
      play "#{@sound_file_dir}/welcome_to_sandbox"
      +pin_authentication
    else
      #Otherwise this is a SIP URI dial from Gizmo5 or elsewhere
      if User.find(:first, :conditions => { :login => extension })
        variable "SANDBOX_USERNAME" => extension
      else
        +unrecognized_username
      end
    end
  end
}

pin_authentication {
  cnt = 0

  while cnt < 3 do
    pin_entered = input :play => "#{@sound_file_dir}/enter_pin_follow_hash", :timeout => 8.seconds 

    if pin_entered != ''    
      user = User.find_by_pin_number(pin_entered)
    end

    if user.nil?
      play "#{@sound_file_dir}/the_pin_is_invalid"
      cnt += 1
      if cnt == 3
        hangup  
      else 
        play "#{@sound_file_dir}/please_try_again"
      end
    else
      variable "SANDBOX_USERNAME" => user.login
      cnt = 3
    end
  end
}

adhearsion_not_running_remotely {
  @sound_file_dir = "sandbox"
  # "Sorry but the sandbox doesn't see your Adhearsion application connected into it. Check that you enabled the sandbox component and that your application has actually been started."
  # "For more help, please visit Adhearsion.com. There you'll find links to the wiki, the IRC channel, and the mailing list."
  play "#{@sound_file_dir}/adhearsion_not_running", "#{@sound_file_dir}/for_more_help"
  hangup
}

unrecognized_username {
  # "Sorry, the username you're calling doesn't seem to have been registered yet on Adhearsion.com. Please visit the site, click the Getting Started link, and sign up for an sandbox account."
  # "For more help, please visit Adhearsion.com. There you'll find links to the wiki, the IRC channel, and the mailing list."
  play "#{@sound_file_dir}/unrecognized_username", "#{@sound_file_dir}/for_more_help"
  hangup
}

require 'md5'

SANDBOX_VERSION = 1.0

def reverse_agi_connection_loop
  loop do
    begin
      socket = TCPSocket.new(host, port)
      socket.puts identifying_hash
      case response = socket.gets.chomp
        when "authentication accepted" 
          ahn_log.sandbox "Authentication accepted"
          start_signal = socket.gets.chomp
          if start_signal
            ahn_log.sandbox "Handing socket off to AGI server."
            Adhearsion::Initializer::AsteriskInitializer.agi_server.serve(socket) rescue nil
            ahn_log.sandbox "AGI server finished serving call. Reconnecting to sandbox."
          else
            ahn_log.sandbox "Remote Asterisk server received no call. Reconnecting..."
          end
        when "authentication failed"
          break
        when /^wait (\d+)$/
          sleep response[/^wait (\d+)$/,1].to_i
        else
          ahn_log.sandbox.error "Invalid login acknowledgement! Skipping sandbox initialization!"
          break
      end
    rescue Errno::ECONNREFUSED
      ahn_log.sandbox.error "Could not connect to the sandbox reverse-AGI server! Skipping sandbox initialization!"
      break
    rescue => e
      ahn_log.error "Unrecognized error: #{e.inspect} \n" + e.backtrace.join("\n")
    end
  end
end

initialization do
  # We shouldn't start initializing until after the AGI server has initialized.
  Events.register_callback(:after_initialized) do
    ahn_log.sandbox "Fetching sandbox connection information"
    begin
      config = YAML.load open("http://sandbox.adhearsion.com/component/#{SANDBOX_VERSION}").read
    rescue SocketError
      ahn_log.sandbox.error "Could not connect to the sandbox server! Skipping sandbox initialization!"
      next
    end

    # The "connect_to" key is what this version supports
    if config.kind_of?(Hash) && config.has_key?("connect_to")
      host, port = config.values_at "host", "port"

      # Part of the AGI-superset protocol we use to log in.
      identifying_hash = MD5.md5(COMPONENTS.sandbox["username"] + ":" + COMPONENTS["password"]).to_s

      if host.nil? || port.nil?
        ahn_log.sandbox.error "Invalid YAML returned from server! Skipping sandbox initialization!"
        next
      end

      Thread.new(&method(:reverse_agi_connection_loop))

    else
      ahn_log.sandbox.error ""
    end
  end
end
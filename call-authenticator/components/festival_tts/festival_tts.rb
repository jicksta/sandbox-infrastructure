# See http://docs.adhearsion.com for more information on how to write components or
# look at the examples in newly-created projects.

methods_for :dialplan do
 def create_festival_sound_file(text)
   filename = COMPONENTS.festival_tts[:sound_dir] + new_guid + ".ulaw"
   system("echo #{text} | text2wave -o #{filename} -otype ulaw")
   return filename.gsub!(".ulaw", "")
 end
end

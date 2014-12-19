begin
  require 'awesome_print'
  AwesomePrint.pry!
rescue LoadError
  puts "no awesome_print :("
end

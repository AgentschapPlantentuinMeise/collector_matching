def gem_installed?(name)
  `gem list #{name} -i`.chomp == "true"
end

unless gem_installed?("dwc_agent")
  system("gem install dwc_agent")
end

require 'dwc_agent'

# Define the input and output file paths
input_file_path = 'data/names.txt'
output_file_path = 'data/output/names_parsed.txt'

# Open the input and output files
File.open(input_file_path, 'r') do |input_file|
  File.open(output_file_path, 'w') do |output_file|
    # Iterate over each line in the input file
    input_file.each_line do |line|
      # Apply DwcAgent.parse to each line
      parsed_line = DwcAgent.parse(line.chomp)

      # Apply DwcAgent.clean to each parsed field
      cleaned_fields = parsed_line.map { |field| DwcAgent.clean(field) }

      # Write the cleaned fields to the output file
      output_file.puts "#{line.chomp}\t#{cleaned_fields.join("\t")}"
    end
  end
end
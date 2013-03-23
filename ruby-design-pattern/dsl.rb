# encoding: utf-8
require 'singleton'
class Backup
  include Singleton

  attr_accessor :backup_directory, :interval
  attr_reader :data_source

  def initialize
    @data_source = []
    @backup_directory = '/backup'
    @interval = 60
  end

  def backup_files
    this_backup_dir = Time.new.ctime.tr(' :', '_')
    this_backup_path = File.join(backup_directory, this_backup_dir)
    @data_source.each {|source| source.backup(this_backup_path)}
  end

  def run
    while true
      backup_files
      sleep(@interval*60)
    end
  end

end

class DataSource
  attr_reader :directory, :finder_expression

  def initialize(directory, finder_expression)
    @directory = directory
    @finder_expression = finder_expression
  end

  def backup(backup_directory)
    files = @finder_expression.evaluate(@directory)
    files.each do |file|
      backup_files(file, backup_directory)
    end
  end

  def backup_file(path, backup_directory)
    copy_path = File.join(backup_directory, path)
    FileUtils.mkdir_p(File.dirname(copy_path))
    FileUtils.cp(path, copy_path)
  end
end

### Load DSL
def backup(dir, finder_expression=All.new)
  Backup.instance.data_source << DataSource.new(dir, finder_expression)
end

def to(backup_directory)
  Backup.instance.backup_directory = backup_directory
end

def interval(minutes)
  Backup.instance.interval = minutes
end

# evaる
eval(File.read('backup.pr'))
Backup.instance.run


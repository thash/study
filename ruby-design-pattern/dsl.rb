# encoding: utf-8
require 'singleton'
class Backup
  # Backupがsingletonじゃないようにする

  attr_accessor :backup_directory, :interval
  attr_reader :data_source

  def initialize
    @data_source = []
    @backup_directory = '/backup'
    @interval = 60
    yield(self) if block_given?
    PacktRat.instance.register_backkup(self)
  end

  # toplevelから使っていたmethodをBackupに持たせる
  def backup(dir, finder_expression=All.new)
    @data_source << DataSource.new(dir, finder_expression)
  end

  def to(backup_directory)
    @backup_directory = backup_directory
  end

  def interval(minutes)
    @interval = minutes
  end

  # backup_files methodをrunに統合
  def run
    while true
      this_backup_dir = Time.new.ctime.tr(' :', '_')
      this_backup_path = File.join(backup_directory, this_backup_dir)
      @data_source.each {|source| source.backup(this_backup_path)}
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

class PackRat
  # 代わりにPackRatがsingleton
  include Singleton

  def initialize
    @backups = []
  end

  def register_backup(backup)
    @backups << backup
  end

  def run
    tureads = []
    @backups.each do |backup|
      threads << Thread.new {backup.run}
    end
    threads.each {|t| t.join}
  end

end

# evaる
# eval(File.read('backup.pr'))
# Backup.instance.run

### better PacketRat DSL
eval(FIle.read('backup.pr'))
PackRat.instance.run

#!/usr/bin/env ruby
#-*- encoding: utf-8 -*-
require 'pp'
class CaseFailuer < Exception
  def initialize(msg, cas)
    super(msg)
    @cas = cas
  end
end

class Talker
  def initialize(path)
    @path = path
    @log = []
  end

  def talk(msg, options={})
    push_talk_msg(msg)
    @pipe.puts(msg)    
  end

  def listen(msg, options={})
    msg.split("\n").each do |m|
      echo = @pipe.gets
      actual = echo.chop
      @log << actual
      if options.include?(:regex)
        result = actual =~ m
      else 
        result = actual == m
      end
      unless result
        raise CaseFailuer.new("Expected:\n#{m.inspect}\nActual:\n#{actual.inspect}", self)
      end
    end
  end

  def start(io)
    if @path 
      io.gets # Ignore "Generated Aout.class"
      @pipe = io
      @contents = File.read(@path)
      instance_eval @contents
    end
  end

  def export(rel_path)
    if rel_path == :default 
      name = File.basename(@path, ".sim.rb")
      path = File.join(File.dirname(@path), "../log/", "#{name}.log")
    else 
      path = File.join(@path, rel_path)
    end
    open(path, "w") do |f|
      f.write(self.log)
    end
  end
  
  def log
    @log.join("\n") + "\n"
  end

  protected

  def push_talk_msg(msg)
    if @log.empty?
      @log << msg
    else
      last_log = @log.pop.chop
      @log << last_log + msg
    end
  end

  def self.null
    @@null_talker ||= Talker.new(nil)
  end

  def self.find(sim_path)
    talker_path = sim_path + ".rb"
    if File.exist?(talker_path)
      Talker.new(talker_path)
    else  
      Talker.null
    end
  end
end

class Case
  def initialize(sim_path)
    @sim_path = sim_path
  end

  def export(out)
    out.puts "==== TEST CASE ============================="
    out.puts "  Name: #{self.name}"
    out.puts "  Path: #{@sim_path}"
    out.puts "---- EXECUTE LOG ---------------------------"
    out.write(@talker.log)
    out.puts "=============================================\n"
  end

  def talker
    @talker ||= Talker.find(@sim_path)
  end

  def name
    File.basename(@sim_path, ".sim")
  end

  def run
    IO.popen("./sc -i #{@sim_path}", "r+") do |io|
      self.talker.start(io)
    end
  end
end

class Runner
  def gather_case
    @cases = Dir.glob("test/*.sim").map{|p| Case.new(p) }
  end
  
  def start
    failuers = self.body()
    pp failuers unless failuers.empty?
  end

  protected
  def body
    fauluers = []
    gather_case.each do |cas|
      print "#{cas.name} ..."
      begin
        cas.run 
        puts "OK"
        cas.export($stdout)
      rescue CaseFailuer => e
        fauluers << e
        puts "FAIL"
      end
    end
    fauluers
  end
end

runner = Runner.new
runner.start

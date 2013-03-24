# encoding: utf-8

# Factory pattern --
# 引数に渡されたsymbolによって作るべきオブジェクトの型を変える
#
# FactoryGirl.create(:article)
# FactoryGirl.create(:user) ...

class Pond
  def initialize(number_animals, animal_class,
                 number_plants,  plant_class)
    @animal_class = animal_class
    @plant_class = plant_class

    @animals = []
    number_animals.times do |i|
      animal = new_organism(:animal, "Animal#{i}")
      @animals << animal
    end

    @plants = []
    number_plants.times do |i|
      plant = new_organism(:plant, "Plant#{i}")
      @plants << plant
    end
  end

  def simulate_one_day
    @plants.each{|plant| plant.grow }
    @animals.each{|animal| animal.speak }
    @animals.each{|animal| animal.eat }
    @animals.each{|animal| animal.sleap }
  end

  # Here is the factory
  def new_organism(type, num)
    if type == :animal
      @animal_class.new(name)
    elsif type == :plant
      @plant_class.new(name)
    else
      raise "Unknown organism type: #{type}"
    end
  end
end

# pond = Pond.new(3, Duck, 2, WaterLily)

# Pond以外にTree, 陸上動物が出てきたらどうする?
# => Abstract Factory patter を利用する. 複数のFactoryを取りまとめる

module BQN
using Logging
using Debugger

abstract type Var end

struct None end
none = None()

mutable struct Ref <: Var
  value::Union{Any,Nothing}
end

struct RefList <: Var
  vec::Vector{Var}
  function RefList(n::Int64)
    v = new(Vector{Var}())
    sizehint!(v.vec, n)
    v
  end
end

struct RefNot <: Var end

struct Env
  parent::Union{Env,Nothing}
  vars::Vector{Var}
end

struct List
  vec::Vector{Any}
  function List(n::Int64)
    v = new(Vector{Any}())
    sizehint!(v.vec, n)
    v
  end
end

function Base.length(coll::List)
  return length(coll.vec)
end
function Base.map(f, coll::List)
  res = List(length(coll))
  for v in coll.vec; push!(res.vec, f(nothing, v)) end
  res
end

function getv(ref::Ref)
  @assert ref.value !== nothing
  ref.value
end

function getv(ref::RefList)
  map(getv, ref.vec)
end

function setn!(ref::Ref, value::Any)
  @assert ref.value == nothing
  ref.value = value
end

function setn!(ref::RefList, value::List)
  @assert length(ref.vec) == length(value.vec)
  for (varitem, valueitem) in zip(ref.vec, value.vec)
    setn!(varitem, valueitem)
  end
end

function setn!(ref::RefNot, value::Any)
end

function setu!(ref::Ref, value::Any)
  @assert ref.value != nothing
  ref.value = value
end

function setu!(ref::RefList, value::List)
  @assert length(ref.vec) == length(value.vec)
  for (varitem, valueitem) in zip(ref.vec, value.vec)
    setu!(varitem, valueitem)
  end
end

function setu!(ref::RefNot, value::Any)
end

call(𝕤::List, 𝕨, 𝕩) = 𝕤
call(𝕤::Int, 𝕨, 𝕩) = 𝕤
call(𝕤::Char, 𝕨, 𝕩) = 𝕤
call(𝕤::String, 𝕨, 𝕩) = 𝕤
call(𝕤, 𝕨, 𝕩) = 𝕤(𝕨, 𝕩)

module Runtime
  import ..List, ..None

  bqnadd(𝕨, 𝕩) = 𝕨 + 𝕩
  bqnsub(𝕨::None, 𝕩::Number) = -𝕩
  bqnsub(𝕨, 𝕩) = 𝕨 - 𝕩
  bqnmul(𝕨, 𝕩) = 𝕨 * 𝕩
  bqndiv(𝕨::None, 𝕩::Number) = 1/𝕩
  bqndiv(𝕨::Number, 𝕩::Number) = 𝕨/𝕩
  bqnroot(root::None, v) = sqrt(v)
  bqnroot(root, v) = v^(1/root)
  bqnabs(𝕨::None, v) = abs(v)
  bqnmin(𝕨::Int64, 𝕩::Number) = min(𝕨, 𝕩)
  bqnmin(𝕨::None, 𝕩::Number) = floor(𝕩)
  bqnnot(𝕨::None, 𝕩::Number) = +(1 - 𝕩)
  bqnnot(𝕨::Number, 𝕩::Number) = 1 + (𝕨 - 𝕩)
  bqnand(𝕨::Number, 𝕩::Number) = 𝕨*𝕩
  bqnor(𝕨::Number, 𝕩::Number) = (𝕨+𝕩)-(𝕨*𝕩)

  bqnidleft(𝕨, 𝕩) = 𝕨
  bqnidright(𝕨, 𝕩) = 𝕩

  function not_implemented(idx)
    return function(w, x)
      println("$(idx) runtime function is not implemented")
      @assert false
    end
  end
end

_runtime = [
  Runtime.bqnadd,
  Runtime.bqnsub,
  Runtime.bqnmul,
  Runtime.bqndiv,
  Runtime.not_implemented(5),
  Runtime.bqnroot,
  Runtime.bqnmin,
  Runtime.not_implemented(8),
  Runtime.bqnabs,
  Runtime.bqnnot,
  Runtime.bqnand,
  Runtime.bqnor,
  Runtime.not_implemented(13),
  Runtime.not_implemented(14),
  Runtime.not_implemented(15),
  Runtime.not_implemented(16),
  Runtime.not_implemented(17),
  Runtime.not_implemented(18),
  Runtime.not_implemented(19),
  Runtime.not_implemented(20),
  Runtime.bqnidleft,
  Runtime.bqnidright,
  Runtime.not_implemented(23),
  Runtime.not_implemented(24),
  Runtime.not_implemented(25),
  Runtime.not_implemented(26),
  Runtime.not_implemented(27),
  Runtime.not_implemented(28),
  Runtime.not_implemented(29),
  Runtime.not_implemented(30),
  Runtime.not_implemented(31),
  Runtime.not_implemented(32),
  Runtime.not_implemented(33),
  Runtime.not_implemented(34),
  Runtime.not_implemented(35),
  Runtime.not_implemented(36),
  Runtime.not_implemented(37),
  Runtime.not_implemented(38),
  Runtime.not_implemented(39),
  Runtime.not_implemented(40),
  Runtime.not_implemented(41),
  Runtime.not_implemented(42),
  Runtime.not_implemented(43),
  Runtime.not_implemented(44),
  Runtime.not_implemented(45),
  Runtime.not_implemented(46),
  Runtime.not_implemented(47),
  Runtime.not_implemented(48),
  Runtime.not_implemented(49),
  Runtime.not_implemented(50),
  Runtime.not_implemented(51),
  Runtime.not_implemented(52),
  Runtime.not_implemented(53),
  Runtime.not_implemented(54),
  Runtime.not_implemented(55),
  Runtime.not_implemented(56),
  Runtime.not_implemented(57),
  Runtime.not_implemented(58),
]

runtime(n::Int64) = _runtime[n + 1]

module Bytecode
  names = Dict(
      0x00 => "PUSH",
      0x01 => "DFND",
      0x06 => "POPS",
      0x07 => "RETN",
      0x0B => "ARRO",
      0x0C => "ARRM",
      0x10 => "FN1C",
      0x11 => "FN2C",
      0x12 => "FN1O",
      0x13 => "FN2O",
      0x14 => "TR2D",
      0x15 => "TR3D",
      0x17 => "TR3O",
      0x1A => "MD1C",
      0x1B => "MD2C",
      0x20 => "VARO",
      0x21 => "VARM",
      0x22 => "VARU",
      0x2C => "NOTM",
      0x30 => "SETN",
      0x31 => "SETU",
      0x32 => "SETM",
      0x33 => "SETC",
  )
end

function vm(src, code, consts, blocks, bodies, toks)
  cbodies = []
  for (idx, body) in enumerate(bodies)
    code_idx, num_vars = body
    push!(cbodies, function(parent, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
            @debug "BODY@$(idx-1) $(num_vars)"
            vars = []
            for _ in 1:num_vars; push!(vars, Ref(nothing)) end
            if num_vars >= 1 vars[1].value = 𝕤 end
            if num_vars >= 2 vars[2].value = 𝕩 end
            if num_vars >= 3 vars[3].value = 𝕨 end
            # TODO: handle 𝕣
            # if num_vars >= 4 vars[4].value = 𝕣 end
            if num_vars >= 5 vars[5].value = 𝕗 end
            if num_vars >= 6 vars[6].value = 𝕘 end
            env = Env(parent, vars)
            run_body(code_idx, env)
          end)
  end

  function run_block(block, env)
    typ, imm, body_idx = block
    @debug "BLOCK type=$(typ) immediate=$(imm) body=$(body_idx)"
    function run(𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
      if isa(body_idx, Int)
        cbodies[body_idx + 1](env, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
      elseif isa(body_idx, Vector)
        ret = nothing
        for body in body_idx
          for idx in body
            # TODO: need to check for PRED/SETH failures here
            ret = cbodies[idx + 1](env, 𝕤, 𝕨, 𝕩, 𝕘, 𝕗)
          end
        end
        @assert ret !== nothing
        ret
      end
    end
    if typ == 0 && imm == 1 # immediate
      run(nothing, nothing, nothing, nothing, nothing)
    elseif typ == 0 && imm == 0 # function
      𝕤 = function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, nothing, nothing) end
      𝕤
    elseif typ == 1 && imm == 1 # mod1 immediate
      function(𝕨, 𝕩) run(nothing, nothing, 𝕩, nothing, nothing) end
    elseif typ == 2 && imm == 1 # mod2 immediate
      function(𝕨, 𝕩) run(nothing, 𝕨, 𝕩, nothing, nothing) end
    elseif typ == 1 && imm == 0 # mod1 deferred
      function(𝕘, 𝕗)
        𝕤 = function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, nothing, 𝕗) end
        𝕤
      end
    elseif typ == 2 && imm == 0 # mod2 deferred
      function(𝕘, 𝕗)
        𝕤 = function(𝕨, 𝕩) run(𝕤, 𝕨, 𝕩, 𝕘, 𝕗) end
        𝕤
      end
    end
  end

  function run_body(code_idx, env)
    stack = []
    s = src
    x = toks
    while true
      instr = code[code_idx + 1]
      if instr == 0x00 # PUSH
        code_idx += 1
        v = consts[code[code_idx + 1] + 1]
        @debug "BYTECODE 00 PUSH $(v)"
        push!(stack, v)
      elseif instr == 0x01 # DFND
        @debug "BYTECODE 01 DFND"
        code_idx += 1
        block = blocks[code[code_idx + 1] + 1]
        push!(stack, run_block(block, env))
      elseif instr == 0x06 # POPS
        @debug "BYTECODE 06 POPS"
        pop!(stack)
      elseif instr == 0x07 # RETN
        @debug "BYTECODE 07 RETN"
        return pop!(stack)
      elseif instr == 0x0B # ARRO
        code_idx += 1
        n = code[code_idx + 1]
        @debug "BYTECODE 0B ARRO N=$(n)"
        v = List(n)
        for i in 1:n
          push!(v.vec, popat!(stack, length(stack) - n + i))
        end
        push!(stack, v)
      elseif instr == 0x0C # ARRM
        @debug "BYTECODE 1C ARRM"
        code_idx += 1
        n = code[code_idx + 1]
        v = RefList(n)
        for i in 1:n
          push!(v.vec, popat!(stack, length(stack) - n + i))
        end
        push!(stack, v)
      elseif instr == 0x10 # FN1C
        @debug "BYTECODE 10 FN1C"
        s, x = pop!(stack), pop!(stack)
        push!(stack, call(s, none, x))
      elseif instr == 0x11 # FN2C
        @debug "BYTECODE 11 FN2C"
        w, s, x = pop!(stack), pop!(stack), pop!(stack)
        push!(stack, call(s, w, x))
      elseif instr == 0x12 # FN1O
        @debug "BYTECODE 12 FN1O"
        s, x = pop!(stack), pop!(stack)
        if x !== none
          push!(stack, call(s, none, x))
        else
          push!(stack, none)
        end
      elseif instr == 0x13 # FN2O
        w, s, x = pop!(stack), pop!(stack), pop!(stack)
        @debug "BYTECODE 13 FN20"
        if x !== none
          push!(stack, call(s, w, x))
        else
          push!(stack, none)
        end
      elseif instr == 0x14 # TR2D
        @debug "BYTECODE 14 TR2D"
        h, 𝕘 = pop!(stack), pop!(stack)
        push!(stack, function(𝕨, 𝕩)
                call(h, none, call(𝕘, 𝕨, 𝕩))
              end)
      elseif instr == 0x15 # TR3D
        @debug "BYTECODE 15 TR3D"
        𝕘, h, 𝕗 = pop!(stack), pop!(stack), pop!(stack)
        push!(stack, function(𝕨, 𝕩)
                𝕩´ = call(𝕗, 𝕨, 𝕩)
                𝕨´ = call(𝕘, 𝕨, 𝕩)
                call(h, 𝕨´, 𝕩´)
              end)
      elseif instr == 0x17 # TR3O
        @debug "BYTECODE 17 TR3O"
        𝕘, h, 𝕗 = pop!(stack), pop!(stack), pop!(stack)
        push!(stack, function(𝕨, 𝕩)
                𝕩´ = call(𝕗, 𝕨, 𝕩)
                𝕨´ = 𝕘 != none ? call(𝕘, 𝕨, 𝕩) : none
                call(h, 𝕨´, 𝕩´)
              end)
      elseif instr == 0x1A # MD1C
        @debug "BYTECODE 1A MD1C"
        f, r = pop!(stack), pop!(stack)
        push!(stack, call(r, none, f))
      elseif instr == 0x1B # MD2C
        @debug "BYTECODE 1B MD2C"
        f, r, g = pop!(stack), pop!(stack), pop!(stack)
        push!(stack, call(r, g, f))
      elseif instr == 0x20 # VARO
        code_idx += 1
        d = code[code_idx + 1]
        code_idx += 1
        i = code[code_idx + 1]
        cenv = env
        while d > 0; cenv = cenv.parent; d -= 1 end
        ref = cenv.vars[i + 1]
        @debug "BYTECODE 20 VARO D=$(d) I=$(i)"
        push!(stack, getv(ref))
      elseif instr == 0x21 # VARM
        code_idx += 1
        d = code[code_idx + 1]
        code_idx += 1
        i = code[code_idx + 1]
        @debug "BYTECODE 21 VARM D=$(d) I=$(i)"
        cenv = env
        while d > 0; cenv = cenv.parent; d -= 1 end
        ref = cenv.vars[i + 1]
        push!(stack, ref)
      elseif instr == 0x22 # VARU
        code_idx += 1
        d = code[code_idx + 1]
        code_idx += 1
        i = code[code_idx + 1]
        cenv = env
        while d > 0; cenv = cenv.parent; d -= 1 end
        ref = cenv.vars[i + 1]
        @debug "BYTECODE 22 VARU D=$(d) I=$(i)"
        # TODO: need to clear the ref
        push!(stack, getv(ref))
      elseif instr == 0x2C # NOTM
        push!(stack, RefNot())
      elseif instr == 0x30 # SETN
        ref, value = pop!(stack), pop!(stack)
        @debug "BYTECODE 30 SETN"
        setn!(ref, value)
        push!(stack, value)
      elseif instr == 0x31 # SETU
        ref, value = pop!(stack), pop!(stack)
        @debug "BYTECODE 31 SETU"
        setu!(ref, value)
        push!(stack, value)
      elseif instr == 0x32 # SETM
        ref, 𝕗, 𝕩 = pop!(stack), pop!(stack), pop!(stack)
        @debug "BYTECODE 32 SETM $(𝕗) $(𝕩)"
        value = call(𝕗, getv(ref), 𝕩)
        setu!(ref, value)
        push!(stack, value)
      elseif instr == 0x33 # SETC
        ref, 𝕗 = pop!(stack), pop!(stack)
        @debug "BYTECODE 33 SETC"
        value = call(𝕗, none, getv(ref))
        setu!(ref, value)
        push!(stack, value)
      else
        println("UNKNOWN BYTECODE 0x$(string(instr, base=16))")
        @assert false
      end
      code_idx += 1
    end
  end

  run_block(blocks[1], Env(nothing, []))
end

function bqncompile(code)
    jlsrc = read(`./BQN/src/cjs.bqn -i $(code)`, String)
    jlcode = eval(Meta.parse(jlsrc))
    return jlcode
end

function bqneval(code)
    jlcode = bqncompile(code)
    boot = eval(jlcode)
    vm(code, boot...)
end

using Test

function test_bytecode(only=nothing)
  cases = [
    (5, "5                       "), #  0 PUSH,  7 RETN
    (3, "4⋄3                     "), #  6 POPS
    (5, "a←5                     "), # 33 VARM, 48 SETN
    (4, "a←5⋄a↩4                 "), # 49 SETU
    (2, "a←2⋄b←3⋄a               "), # 32 VARO
    (1, "a←1⋄A 4                 "), # 16 FN1C
    (2, "a←2⋄3 A 4               "), # 17 FN2C
    (6, "{𝕩}6                    "), #  1 DFND
    (3, "A←{𝕨}⋄3 A 4             "), #    dyadic block function
    (2, "a‿b←7‿2⋄b               "), # 11 ARRO, 12 ARRM
    (7, "a‿b←7‿2⋄a               "), # 11 ARRO, 12 ARRM
    (2, "·‿b←7‿2⋄b               "), # 36 NOTM
    (2, "0{𝕨𝕏1}2                 "), # 19 FN2O (doesn't test call type)
    (5, "{({𝕨}𝕨)𝕏𝕩}5             "), # 18 FN1O not called
    (8, "{𝕩{a‿b←𝕨}𝕨,𝕩}8          "), # 19 FN2O not called
    (4, "4{𝔽}                    "), # 26 MD1C
    (6, "4{𝔽⋄𝕩}6                 "), #    deferred modifier
    (1, "3{𝔾}{𝕩} 1               "), # 27 MD2C
    (2, "(2{𝔽}{𝕩})3              "), # 20 TR2D
    (3, "3({a‿b←𝕩⋄a}{𝕨‿𝕩})4      "), #    dyadic 2-train
    (4, "4({𝕨‿𝕩}{𝕩}{𝕨})5         "), # 21 TR3D
    (2, "a‿b←(2{𝕨‿𝕩}{𝕩})5⋄a      "), #    monadic, data in left branch
    (2, "({a↩2⋄𝕩}{𝕩⋄a}{a↩3⋄𝕩})a←4"), # ordering
    (8, "a←3⋄a{𝕩}↩8⋄a            "), # 50 SETM
    (5, "a←4⋄a{𝕨⋄5}↩6            "), #    returns new value
    (1, "a←3⋄a{𝕩⋄1}↩⋄a           "), # 51 SETC
    (4, "a‿b←2‿1⋄a‿b{𝕩‿𝕨}↩4⋄a    "), #    lists

    (7, "     {𝕨{a←𝕩⋄{a↩𝕩}𝕨⋄a}𝕩}7"),  # 19 FN2O call type
    (3, "    3{𝕨{a←𝕩⋄{a↩𝕩}𝕨⋄a}𝕩}7"),  # 19 FN2O call type
    (7, "{𝕏0} {𝕨{a←𝕩⋄{a↩𝕩}𝕨⋄a}𝕏}7"),  # 23 TR3O call type
    (3, "{𝕏0}3{𝕨{a←𝕩⋄{a↩𝕩}𝕨⋄a}𝕏}7"),  # 23 TR3O call type

    (1, "a←1⋄{a←2}⋄a"),
    (2, "a←1⋄{a↩2}⋄a"),
    (6, "f‿g←{a←2⋄{a↩𝕩}‿{𝕩⋄a}}⋄F 6⋄G 0"),
    (5, "L←{𝕩{𝕏𝕗}}⋄{𝕏𝕤}L L L 5"),
    (3, "_l←{𝕩{𝕏𝕗} 𝔽}⋄{𝕏𝕤} {𝕩}_l 3 _l 5"),
    (1, "1{𝕨}{𝔽{𝕩𝔽𝕨}𝔾𝔽}{𝕩}0"),    # 0≠1 via Church booleans
    (2, "0‿(0‿{𝕩}){{a‿b←𝕩⋄t←𝕤⋄{𝕤⋄T↩{𝕤⋄{a‿b←𝕩⋄a}}}{B𝕗}0⋄(T b){a‿b←𝕩⋄𝔽b}}𝕗} 0‿(1‿(2‿(3‿(4‿{𝕩}))))"),
  ]
  for (idx, (expected, code)) in enumerate(cases)
    if only !== nothing && !(idx in only); continue end
    println("=== TEST@$(idx) $(code)")
    got = bqneval(code)
    Test.@test expected == got
  end
end

function test_simple(only=nothing)
  cases = [
          (2    , "1+1"),
          (-2   , "1⌊-2"),
          (-1   , "-2⌊1"),
          (1    , "(÷2)+(÷3)+(÷6)"),
          (4    , "⊢4⊣5"),
          (0    , "sq←√5,⌊9×|5-sq×sq"),
          (-0.5 , "((-3)+√(3×3)-4×2×1)÷2×2"),
          (-0.5 , "a←2,b←3,c←1⋄((-b)+√(b×b)-4×a×c)÷2×a"),
          (-0.5 , "b←1+a←1+c←1⋄((-b)+√(b×b)-4×a×c)÷2×a"),
          (-0.5 , "b←3⋄⊢d←(b×b)-4×2×1⋄((-b)+√d)÷2×2"),
          (6    , "a←3,b←4,c←5⋄⊣s←(÷2)×a+b+c⋄√s×(s-a)×(s-b)×(s-c)"),
          (3.1415, "t←2×5⋄3+(1+(4+(1+5÷t)÷t)÷t)÷t"),
          (3.1415, "3+(1+(4+(1+5÷10)÷10)÷10)÷10"),
          (3    , "√25-16"),
          (0.25 , "¬15÷20"),
          (0    , "(3∧4)-¬(¬3)∨(¬4)"),
          (1    , "p←¬q←÷4⋄(q∧q)+(p∨p)"),
          (109  , "105¬-3"),
          (-0.5 , "{{-3}+√{3×3}-4×2×1}÷2×2"),
          (1    , "{a←1⋄{a←2}⋄a}"),
    ]
  for (idx, (expected, code)) in enumerate(cases)
    if only !== nothing && !(idx in only); continue end
    println("=== TEST@$(idx) $(code)")
    got = bqneval(code)
    Test.@test expected == got
  end
end

end

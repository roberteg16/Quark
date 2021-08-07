# QUARK

## How to build

Quark has some dependencies so in order to get it to compile some dependecies must be installed before.

Requeriments:
- Flex >=2.6.4
- Bison >=3.5.1
- libomp-dev
- CMake >=3.13.4
- clang >= 11

Once the dependencies are installed clone the respository and then:
- `mkdir build && cd build`
- `cmake -G <YourPreferedGenerator> -C ../quark/cmake/caches/Debug.cmake -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ ../llvm`
- `<YourPreferedGenerator> quark`
- [OPTIONAL] Run tests: `<YourPreferedGenerator> check-quark`

Once it finished to compile a source:
- `quark <sourcefile.qk>` (Will generate a file named `ir.ll`)
- `<YourPreferedGenerator> llc-12`
- `llc-12 -filetype=obj ir.ll -o ir.o && clang ir.o -fopenmp=libiomp5 -o <executable name>`
- `./<executable name>`

## Sample of syntax

```
type Car {
  Owner *i8
  Brand *i8
}

fn (car *Car) dump: -> void {
  print <- "The car of brand '%s' belongs to '%s'\n" {car->Owner, car->Brand};
}

fn dumpAllCars: cars *Car, numberOfCars i32 -> void {
  print <- "Total number of cars: %d\n" {numberOfCars};
  
  for (var i := 0; i < numberOfCars; i = i + 1) {
    print <- "\t" {};
    cars[i].dump();
  }
}

fn main: argc i32, argv **i8 -> i32 {
  
  // -1 to not take into account the executable
  var argcWithoutProgram := argc - 1;

  if (argcWithoutProgram % 2 != 0) {
    print <- "Missing a onwer or brand entry\n" {}; 
    ret 1;
  } 

  var numberOfCars := argcWithoutProgram / 2;
  var allCars := alloc Car[numberOfCars];

  for (var i := 0; i < numberOfCars; i = i + 1) {
    allCars[i].Owner = argv[(i*2)+1];
    allCars[i].Brand = argv[(i*2)+2];
  }

  dumpAllCars(allCars, numberOfCars);

  dealloc allCars;

  ret 0;
}
```

Generated AST (quark --emit-ast <source_file.qk>):
```
`-SourceModule: 
  |-TypeDecl: Car <file:'largeExample.qk' <line:1 col:1>-<line:4 col:2>>
  | |-TypeFieldDecl: Owner *i8 <line:2 <col:3 col:12>>
  | `-TypeFieldDecl: Brand *i8 <line:3 <col:3 col:12>>
  |-MethodDecl: (*Car) dump: (  ) -> void <line:6 col:1>-<line:8 col:2>
  | |-VarDecl: (reciv) car *Car <col:5 col:13>
  | `-PrintStmt: <line:7 <col:3 col:79>>
  |   |-StringExpr: *u8 "The car of brand '%s' belongs to '%s'\n" <col:12 col:53>
  |   |-ImplicitCastExpr: LValueToRValue <col:55 col:58>
  |   | `-MemberExpr: 'Owner' *i8 <col:55 col:58>
  |   |   `-ImplicitCastExpr: LValueToRValue <col:55 col:58>
  |   |     `-VarRefExpr: *Car car <col:55 col:58>
  |   `-ImplicitCastExpr: LValueToRValue <col:67 col:70>
  |     `-MemberExpr: 'Brand' *i8 <col:67 col:70>
  |       `-ImplicitCastExpr: LValueToRValue <col:67 col:70>
  |         `-VarRefExpr: *Car car <col:67 col:70>
  |-FuncDecl: dumpAllCars: ( cars *Car, numberOfCars i32 ) -> void <line:10 col:1>-<line:17 col:2>
  | |-VarDecl: (param) cars *Car <col:17 col:26>
  | |-VarDecl: (param) numberOfCars i32 <col:28 col:44>
  | |-PrintStmt: <line:11 <col:3 col:56>>
  | | |-StringExpr: *u8 "Total number of cars: %d\n" <col:12 col:40>
  | | `-ImplicitCastExpr: LValueToRValue <col:42 col:54>
  | |   `-VarRefExpr: i32 numberOfCars <col:42 col:54>
  | `-ForStmt: SeqLoop <line:13 col:3>-<line:16 col:4>
  |   |-VarDeclStmt: <col:8 col:18>
  |   | |-VarDecl: (local) i i32 <col:8 col:18>
  |   | `-IntegerExpr: i32 0 <col:17>
  |   |-BinaryExpr: b1 (<) <col:20 col:36>
  |   | |-ImplicitCastExpr: LValueToRValue <col:20>
  |   | | `-VarRefExpr: i32 i <col:20>
  |   | `-ImplicitCastExpr: LValueToRValue <col:24 col:36>
  |   |   `-VarRefExpr: i32 numberOfCars <col:24 col:36>
  |   |-BinaryExpr: i32 (=) <col:38 col:47>
  |   | |-VarRefExpr: i32 i <col:38>
  |   | `-BinaryExpr: i32 (+) <col:42 col:47>
  |   |   |-ImplicitCastExpr: LValueToRValue <col:42>
  |   |   | `-VarRefExpr: i32 i <col:42>
  |   |   `-IntegerExpr: i32 1 <col:46>
  |   `-BlockStmt: <line:13 col:49>-<line:16 col:4>
  |     |-PrintStmt: <line:14 <col:5 col:22>>
  |     | `-StringExpr: *u8 "\t" <col:14 col:18>
  |     `-ExprStmt: <line:15 <col:5 col:20>>
  |       `-MemberCallExpr: (rec: Car) dump () -> void <col:5 col:9>
  |         `-ArrayAccessExpr: Car <col:5 col:9>
  |           |-ImplicitCastExpr: LValueToRValue <col:5 col:9>
  |           | `-VarRefExpr: *Car cars <col:5 col:9>
  |           `-ImplicitCastExpr: LValueToRValue <col:10>
  |             `-VarRefExpr: i32 i <col:10>
  `-FuncDecl: main: ( argc i32, argv **i8 ) -> i32 <line:19 col:1>-<line:42 col:2>
    |-VarDecl: (param) argc i32 <col:10 col:18>
    |-VarDecl: (param) argv **i8 <col:20 col:29>
    |-VarDeclStmt: <line:22 <col:3 col:37>>
    | |-VarDecl: (local) argcWithoutProgram i32 <col:3 col:37>
    | `-BinaryExpr: i32 (-) <col:29 col:37>
    |   |-ImplicitCastExpr: LValueToRValue <col:29 col:33>
    |   | `-VarRefExpr: i32 argc <col:29 col:33>
    |   `-IntegerExpr: i32 1 <col:36>
    |-IfStmt: <line:24 col:3>-<line:27 col:4>
    | |-BinaryExpr: b1 (!=) <col:7 col:34>
    | | |-BinaryExpr: i32 (%) <col:7 col:29>
    | | | |-ImplicitCastExpr: LValueToRValue <col:7 col:25>
    | | | | `-VarRefExpr: i32 argcWithoutProgram <col:7 col:25>
    | | | `-IntegerExpr: i32 2 <col:28>
    | | `-IntegerExpr: i32 0 <col:33>
    | `-BlockStmt: <line:24 col:36>-<line:27 col:4>
    |   |-PrintStmt: <line:25 <col:5 col:52>>
    |   | `-StringExpr: *u8 "Missing a onwer or brand entry\n" <col:14 col:48>
    |   `-RetStmt: <line:26 <col:5 col:11>>
    |     `-IntegerExpr: i32 1 <col:9>
    |-VarDeclStmt: <line:29 <col:3 col:45>>
    | |-VarDecl: (local) numberOfCars i32 <col:3 col:45>
    | `-BinaryExpr: i32 (/) <col:23 col:45>
    |   |-ImplicitCastExpr: LValueToRValue <col:23 col:41>
    |   | `-VarRefExpr: i32 argcWithoutProgram <col:23 col:41>
    |   `-IntegerExpr: i32 2 <col:44>
    |-VarDeclStmt: <line:30 <col:3 col:41>>
    | |-VarDecl: (local) allCars *Car <col:3 col:41>
    | `-AllocExpr: *Car <col:18 col:41>
    |   `-ImplicitCastExpr: LValueToRValue <col:28 col:40>
    |     `-VarRefExpr: i32 numberOfCars <col:28 col:40>
    |-ForStmt: SeqLoop <line:32 col:3>-<line:35 col:4>
    | |-VarDeclStmt: <col:8 col:18>
    | | |-VarDecl: (local) i i32 <col:8 col:18>
    | | `-IntegerExpr: i32 0 <col:17>
    | |-BinaryExpr: b1 (<) <col:20 col:36>
    | | |-ImplicitCastExpr: LValueToRValue <col:20>
    | | | `-VarRefExpr: i32 i <col:20>
    | | `-ImplicitCastExpr: LValueToRValue <col:24 col:36>
    | |   `-VarRefExpr: i32 numberOfCars <col:24 col:36>
    | |-BinaryExpr: i32 (=) <col:38 col:47>
    | | |-VarRefExpr: i32 i <col:38>
    | | `-BinaryExpr: i32 (+) <col:42 col:47>
    | |   |-ImplicitCastExpr: LValueToRValue <col:42>
    | |   | `-VarRefExpr: i32 i <col:42>
    | |   `-IntegerExpr: i32 1 <col:46>
    | `-BlockStmt: <line:32 col:49>-<line:35 col:4>
    |   |-ExprStmt: <line:33 <col:5 col:38>>
    |   | `-BinaryExpr: *i8 (=) <col:5 col:37>
    |   |   |-MemberExpr: 'Owner' *i8 <col:5 col:12>
    |   |   | `-ArrayAccessExpr: Car <col:5 col:12>
    |   |   |   |-ImplicitCastExpr: LValueToRValue <col:5 col:12>
    |   |   |   | `-VarRefExpr: *Car allCars <col:5 col:12>
    |   |   |   `-ImplicitCastExpr: LValueToRValue <col:13>
    |   |   |     `-VarRefExpr: i32 i <col:13>
    |   |   `-ImplicitCastExpr: LValueToRValue <col:24 col:28>
    |   |     `-ArrayAccessExpr: *i8 <col:24 col:28>
    |   |       |-ImplicitCastExpr: LValueToRValue <col:24 col:28>
    |   |       | `-VarRefExpr: **i8 argv <col:24 col:28>
    |   |       `-BinaryExpr: i32 (+) <col:29 col:36>
    |   |         |-BinaryExpr: i32 (*) <col:30 col:33>
    |   |         | |-ImplicitCastExpr: LValueToRValue <col:30>
    |   |         | | `-VarRefExpr: i32 i <col:30>
    |   |         | `-IntegerExpr: i32 2 <col:32>
    |   |         `-IntegerExpr: i32 1 <col:35>
    |   `-ExprStmt: <line:34 <col:5 col:38>>
    |     `-BinaryExpr: *i8 (=) <col:5 col:37>
    |       |-MemberExpr: 'Brand' *i8 <col:5 col:12>
    |       | `-ArrayAccessExpr: Car <col:5 col:12>
    |       |   |-ImplicitCastExpr: LValueToRValue <col:5 col:12>
    |       |   | `-VarRefExpr: *Car allCars <col:5 col:12>
    |       |   `-ImplicitCastExpr: LValueToRValue <col:13>
    |       |     `-VarRefExpr: i32 i <col:13>
    |       `-ImplicitCastExpr: LValueToRValue <col:24 col:28>
    |         `-ArrayAccessExpr: *i8 <col:24 col:28>
    |           |-ImplicitCastExpr: LValueToRValue <col:24 col:28>
    |           | `-VarRefExpr: **i8 argv <col:24 col:28>
    |           `-BinaryExpr: i32 (+) <col:29 col:36>
    |             |-BinaryExpr: i32 (*) <col:30 col:33>
    |             | |-ImplicitCastExpr: LValueToRValue <col:30>
    |             | | `-VarRefExpr: i32 i <col:30>
    |             | `-IntegerExpr: i32 2 <col:32>
    |             `-IntegerExpr: i32 2 <col:35>
    |-ExprStmt: <line:37 <col:3 col:38>>
    | `-FunctionCallExpr: dumpAllCars (*Car, i32) -> void <col:3 col:37>
    |   |-ImplicitCastExpr: LValueToRValue <col:15 col:22>
    |   | `-VarRefExpr: *Car allCars <col:15 col:22>
    |   `-ImplicitCastExpr: LValueToRValue <col:24 col:36>
    |     `-VarRefExpr: i32 numberOfCars <col:24 col:36>
    |-DeallocStmt: <line:39 <col:3 col:19>>
    | `-VarRefExpr: *Car allCars <col:11 col:18>
    `-RetStmt: <line:41 <col:3 col:9>>
      `-IntegerExpr: i32 0 <col:7>
```

Generated IR (quark --emit-ir <source_file.qk>):
```
; ModuleID = 'largeExample.qk'
source_filename = "largeExample.qk"

%Car = type { i8*, i8* }

@0 = private unnamed_addr constant [39 x i8] c"The car of brand '%s' belongs to '%s'\0A\00", align 1
@1 = private unnamed_addr constant [26 x i8] c"Total number of cars: %d\0A\00", align 1
@2 = private unnamed_addr constant [2 x i8] c"\09\00", align 1
@3 = private unnamed_addr constant [32 x i8] c"Missing a onwer or brand entry\0A\00", align 1

define void @dump__rec__ptr__Car(%Car* %0) {
entry:
  %car.var = alloca %Car*, align 8
  store %Car* %0, %Car** %car.var, align 8
  %1 = load %Car*, %Car** %car.var, align 8
  %2 = getelementptr inbounds %Car, %Car* %1, i32 0, i32 0
  %3 = load i8*, i8** %2, align 8
  %4 = load %Car*, %Car** %car.var, align 8
  %5 = getelementptr inbounds %Car, %Car* %4, i32 0, i32 1
  %6 = load i8*, i8** %5, align 8
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @0, i32 0, i32 0), i8* %3, i8* %6)
  br label %func.end

func.end:                                         ; preds = %entry
  ret void
}

declare i32 @printf(i8*, ...)

define void @dumpAllCars__ptr__Cari32(%Car* %0, i32 %1) {
entry:
  %cars.var = alloca %Car*, align 8
  %numberOfCars.var = alloca i32, align 4
  %i.var = alloca i32, align 4
  store %Car* %0, %Car** %cars.var, align 8
  store i32 %1, i32* %numberOfCars.var, align 4
  %2 = load i32, i32* %numberOfCars.var, align 4
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([26 x i8], [26 x i8]* @1, i32 0, i32 0), i32 %2)
  br label %for.init

for.init:                                         ; preds = %entry
  store i32 0, i32* %i.var, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %for.init
  %4 = load i32, i32* %i.var, align 4
  %5 = load i32, i32* %numberOfCars.var, align 4
  %6 = icmp slt i32 %4, %5
  br i1 %6, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @2, i32 0, i32 0))
  %8 = load %Car*, %Car** %cars.var, align 8
  %9 = load i32, i32* %i.var, align 4
  %10 = getelementptr inbounds %Car, %Car* %8, i32 %9
  call void @dump__rec__ptr__Car(%Car* %10)
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %11 = load i32, i32* %i.var, align 4
  %12 = add i32 %11, 1
  store i32 %12, i32* %i.var, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  br label %func.end

func.end:                                         ; preds = %for.end
  ret void
}

define i32 @main(i32 %0, i8** %1) {
entry:
  %argc.var = alloca i32, align 4
  %argv.var = alloca i8**, align 8
  %argcWithoutProgram.var = alloca i32, align 4
  %numberOfCars.var = alloca i32, align 4
  %allCars.var = alloca %Car*, align 8
  %i.var = alloca i32, align 4
  %ret.var = alloca i32, align 4
  store i32 %0, i32* %argc.var, align 4
  store i8** %1, i8*** %argv.var, align 8
  %2 = load i32, i32* %argc.var, align 4
  %3 = sub i32 %2, 1
  store i32 %3, i32* %argcWithoutProgram.var, align 4
  br label %if.cond

if.cond:                                          ; preds = %entry
  %4 = load i32, i32* %argcWithoutProgram.var, align 4
  %5 = srem i32 %4, 2
  %6 = icmp ne i32 %5, 0
  br i1 %6, label %if.code, label %if.end

if.code:                                          ; preds = %if.cond
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([32 x i8], [32 x i8]* @3, i32 0, i32 0))
  store i32 1, i32* %ret.var, align 4
  br label %func.end

if.end:                                           ; preds = %if.cond
  %8 = load i32, i32* %argcWithoutProgram.var, align 4
  %9 = sdiv i32 %8, 2
  store i32 %9, i32* %numberOfCars.var, align 4
  %10 = load i32, i32* %numberOfCars.var, align 4
  %mallocsize = mul i32 %10, trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32)
  %malloccall = tail call i8* @malloc(i32 %mallocsize)
  %11 = bitcast i8* %malloccall to %Car*
  store %Car* %11, %Car** %allCars.var, align 8
  br label %for.init

for.init:                                         ; preds = %if.end
  store i32 0, i32* %i.var, align 4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %for.init
  %12 = load i32, i32* %i.var, align 4
  %13 = load i32, i32* %numberOfCars.var, align 4
  %14 = icmp slt i32 %12, %13
  br i1 %14, label %for.body, label %for.end

for.body:                                         ; preds = %for.cond
  %15 = load %Car*, %Car** %allCars.var, align 8
  %16 = load i32, i32* %i.var, align 4
  %17 = getelementptr inbounds %Car, %Car* %15, i32 %16
  %18 = getelementptr inbounds %Car, %Car* %17, i32 0, i32 0
  %19 = load i8**, i8*** %argv.var, align 8
  %20 = load i32, i32* %i.var, align 4
  %21 = mul i32 %20, 2
  %22 = add i32 %21, 1
  %23 = getelementptr inbounds i8*, i8** %19, i32 %22
  %24 = load i8*, i8** %23, align 8
  store i8* %24, i8** %18, align 8
  %25 = load %Car*, %Car** %allCars.var, align 8
  %26 = load i32, i32* %i.var, align 4
  %27 = getelementptr inbounds %Car, %Car* %25, i32 %26
  %28 = getelementptr inbounds %Car, %Car* %27, i32 0, i32 1
  %29 = load i8**, i8*** %argv.var, align 8
  %30 = load i32, i32* %i.var, align 4
  %31 = mul i32 %30, 2
  %32 = add i32 %31, 2
  %33 = getelementptr inbounds i8*, i8** %29, i32 %32
  %34 = load i8*, i8** %33, align 8
  store i8* %34, i8** %28, align 8
  br label %for.inc

for.inc:                                          ; preds = %for.body
  %35 = load i32, i32* %i.var, align 4
  %36 = add i32 %35, 1
  store i32 %36, i32* %i.var, align 4
  br label %for.cond

for.end:                                          ; preds = %for.cond
  %37 = load %Car*, %Car** %allCars.var, align 8
  %38 = load i32, i32* %numberOfCars.var, align 4
  call void @dumpAllCars__ptr__Cari32(%Car* %37, i32 %38)
  %39 = load %Car*, %Car** %allCars.var, align 8
  %40 = bitcast %Car* %39 to i8*
  tail call void @free(i8* %40)
  store i32 0, i32* %ret.var, align 4
  br label %func.end

func.end:                                         ; preds = %for.end, %if.code
  %41 = load i32, i32* %ret.var, align 4
  ret i32 %41
}

declare noalias i8* @malloc(i32)

declare void @free(i8*)
```

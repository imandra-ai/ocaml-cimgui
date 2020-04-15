
let prelude =
  {|
#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS 1
#include "vulkan/vulkan.h"
#include "cimgui.h"
#include "cimgui_impl.h"
#include <stdbool.h>

typedef struct ImGui_ImplVulkan_InitInfo ImGui_ImplVulkan_InitInfo;
|}

let () =
  let genml = ref false in
  let genc = ref false in
  Arg.parse [
    "-ml", Arg.Set genml, " gen ml";
    "-c", Arg.Set genc, " gen c";
  ] (fun _ -> failwith "bluh") "";
  if !genml = !genc then failwith "give exactly one arg";
  let prefix = "cimgui_ml_vulkan" in
  if !genml then (
    Cstubs.write_ml Format.std_formatter ~prefix (module Imgui_vulkan_ffi_f.Make)
  ) else (
    print_endline prelude;
    Cstubs.write_c Format.std_formatter ~prefix (module Imgui_vulkan_ffi_f.Make)
  )

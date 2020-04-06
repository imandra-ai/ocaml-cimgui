open Ctypes

module Types = Imgui_sdl2_types_f.Make(Imgui_sdl2_generated_types)

(* module Types = Imgui_sdl2_types_f.Make(Imgui_sdl2_generated_types) *)

module Make(F : Cstubs.FOREIGN) = struct
  open! Types
  open! F

  let init_for_d3d =
    foreign "ImGui_ImplSDL2_InitForD3D"
      (ptr window @-> returning bool)

  (** @param sdl_gl_context the openGL context *)
  let init_for_opengl =
    foreign "ImGui_ImplSDL2_InitForOpenGL"
      (ptr window @-> ptr void @-> returning bool)

  let init_for_vulkan =
    foreign "ImGui_ImplSDL2_InitForVulkan"
      (ptr window @-> returning bool)

  let new_frame =
    foreign "ImGui_ImplSDL2_NewFrame"
      (ptr window @-> returning void)

  let process_event =
    foreign "ImGui_ImplSDL2_ProcessEvent"
      (ptr event @-> returning bool)
end

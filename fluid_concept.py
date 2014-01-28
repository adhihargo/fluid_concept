# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####

# Author: Adhi Hargo (cadmus.sw@gmail.com)

import bpy
import os
import re
import subprocess
import tempfile
from bpy.types import Operator, Menu
from bpy.props import BoolProperty, EnumProperty, IntProperty, StringProperty

bl_info = {
    "name": "ADH Fluid Concept",
    "author": "Adhi Hargo",
    "version": (1, 0, 0),
    "blender": (2, 68, 0),
    "location": "View3D > Tools > ADH Fluid Concept",
    "description": "In-house production tools.",
    "warning": "",
    "wiki_url": "https://github.com/adhihargo/fluid_concept",
    "tracker_url": "https://github.com/adhihargo/fluid_concept/issues",
    "category": "Sequencer"}

PRJ_IMG_PREFIX = 'PRJ_IMG_'
PRJ_UVMAP_PREFIX = 'PRJ_UVMAP_'
PRJ_MAT_PREFIX = 'PRJ_MAT_'
PRJ_TEX_PREFIX = 'PRJ_TEX_'
BASE_LAYER_SEPARATOR = '_'
HAS_XCFTOOLS = any(filter(lambda p: os.path.exists(os.path.join(p, 'xcf2png')),
                          os.environ.get('PATH', '').split(os.pathsep)))
XCFINFO_OUTPUT_RE = re.compile(r"""
(?P<visibility>[+-])\s
(?P<width>\d+)x(?P<height>\d+)\+(?P<offset_x>\d+)\+(?P<offset_y>\d+)\s
(?P<color_mode>\S+)\s
(?P<layer_mode>\S+)\s
(?P<layer_name>.+)$""", re.VERBOSE)

def edit_image_file(context, filepath, external_editor = True):
    image_editor = context.user_preferences.filepaths.image_editor
    if image_editor and external_editor:
        proc = subprocess.Popen([image_editor, filepath])
        return

    image = bpy.data.images.load(filepath)

    area_list = [a for a in context.screen.areas if a != context.area and
                 not a.type in ['SEQUENCE_EDITOR', 'INFO']]
    image_area = area_list[-1] if area_list else context.area
    image_area.type = 'IMAGE_EDITOR'
    image_space = image_area.spaces.active
    image_space.image = image
    image_space.mode = 'PAINT'

def get_temp_dir():
    temp_dir = bpy.context.user_preferences.filepaths.temporary_directory
    if not temp_dir:
        temp_dir = tempfile.gettempdir()

    return temp_dir

def get_xcf_layers(filepath):
    if not (HAS_XCFTOOLS and os.path.exists(filepath)):
        return []

    status, output_str = subprocess.getstatusoutput('xcfinfo "%s"' % filepath)
    if(status != 0):
        return []

    output = [m.group('layer_name')
              for m in map(lambda l: XCFINFO_OUTPUT_RE.match(l),
                           output_str.split('\n')[1:]) if m]
    return output

def get_master_file(filepath):
    filepath = bpy.path.abspath(filepath)
    dirname, filename = os.path.split(filepath)
    basename = os.path.splitext(filename)[0]
    if not filepath:
        return filepath

    master_filepath = os.path.join(dirname, basename + '.xcf')
    if not os.path.exists(master_filepath):
        # Assume filename is XCFNAME_LAYERNAME.EXT, try if XCFNAME.xcf matches
        rsep = basename.rfind(BASE_LAYER_SEPARATOR)
        while rsep != -1:
            master_filepath = os.path.join(dirname, basename[:rsep] + '.xcf')
            if os.path.exists(master_filepath):
                break
            rsep = basename.rfind(BASE_LAYER_SEPARATOR, 0, rsep)
    if not os.path.exists(master_filepath):
        master_filepath = filepath

    return master_filepath

def get_background_enums(self, context):
    space = context.space_data
    enums = [(str(i), b.image.name, 'Image "%s"' % b.image.name)
             for i, b in enumerate(space.background_images)
             if b.source == 'IMAGE']
    return enums

def get_scene_enums(self, context):
    enums = [(s.name, s.name, 'Scene "%s"' % s.name) for s in bpy.data.scenes]
    if not enums:
        enums.append((context.scene.name, "(no other scene)",
                      "There is no other scene to choose from."))
    return enums

def get_topmost_channel(sequences, frame_min, frame_max):
    frame_range = range(frame_min, frame_max)
    sequences_in_range = list(filter(lambda s:\
                                         s.frame_final_start in frame_range or\
                                         s.frame_final_end in frame_range,
                                     sequences))
    return max(map(lambda s: s.channel, sequences_in_range))\
        if sequences_in_range else 1

def get_uvmap_image(uvmap):
    image = None
    for uvmap_poly in uvmap.data:
        if uvmap_poly.image:
            image = uvmap_poly.image
            break

    return image

def get_fcurve(action, sequence):
    channel = 'volume' if sequence.type == 'SOUND' else 'blend_alpha'
    data_path = 'sequence_editor.sequences_all["%s"].%s' % (
        sequence.name, channel)

    fcurve = None
    for f in action.fcurves:
        if f.data_path == data_path:
            fcurve = f
            break
    if not fcurve:
        fcurve = action.fcurves.new(data_path)

    return fcurve

def render_image(context, filepath, scale = 100, scene = None, opengl = False):
    prev = {}
    # Save, render...
    if context.space_data.type == 'VIEW_3D':
        prev['view_persp'] = context.space_data.region_3d.view_perspective
    prev['scene'] = context.screen.scene
    if scene:
        context.screen.scene = scene

    render_settings = context.scene.render
    prev['filepath'] = render_settings.filepath
    prev['fileformat'] = render_settings.image_settings.file_format
    prev['render_ratio'] = render_settings.resolution_percentage
    prev['alpha_mode'] = render_settings.alpha_mode
    prev['color_mode'] = render_settings.image_settings.color_mode
    render_settings.filepath = filepath
    render_settings.image_settings.file_format = 'PNG'
    render_settings.resolution_percentage = scale
    render_settings.alpha_mode = 'TRANSPARENT'
    render_settings.image_settings.color_mode = 'RGBA'

    render_func = bpy.ops.render.opengl if opengl else bpy.ops.render.render
    render_func(animation = False, write_still = True)

    # ... restore.
    render_settings.filepath = prev['filepath']
    render_settings.image_settings.file_format = prev['fileformat']
    render_settings.resolution_percentage = prev['render_ratio']
    render_settings.alpha_mode = prev['alpha_mode']
    render_settings.image_settings.color_mode = prev['color_mode']

    context.screen.scene = prev['scene']
    if context.space_data.type == 'VIEW_3D':
        context.space_data.region_3d.view_perspective = prev['view_persp']

class OBJECT_OT_adh_copy_action(Operator):
    bl_idname = 'object.adh_copy_action'
    bl_label = 'Copy All Actions'
    bl_options = {'REGISTER', 'UNDO'}

    offset_frame = IntProperty(name = "Offset", subtype = "TIME")

    @classmethod
    def poll(self, context):
        return context.active_object

    def execute(self, context):
        obj = context.active_object
        other_objs = sorted(
            [o for o in context.selected_objects if o != obj],
            key = lambda o: ((o.location + o.delta_location) -
                             (obj.location + obj.delta_location)).length)
        actions = {}

        for owner, key in [(obj, "object"), (obj.data, "data")]:
            if owner.animation_data and owner.animation_data.action:
                actions[key] = owner.animation_data.action

        offset = self.offset_frame
        for o in other_objs:
            key = "location"
            delta = ((getattr(o, key) + getattr(o, "delta_"+key)) -
                     (getattr(obj, key) + getattr(obj, "delta_"+key)))
            setattr(o, "delta_"+key, delta)

            for owner, key in [(o, "object"), (o.data, "data")]:
                if not actions.get(key, None):
                    continue
                owner.animation_data_create()
                owner.animation_data.action = None

                action_name = actions[key].name + "_copy_" + o.name
                action = bpy.data.actions.get(action_name, None)
                if action:
                    bpy.data.actions.remove(action)
                action = actions[key].copy()
                action.name = action_name
                owner.animation_data.action = action

                fcurves = action.fcurves
                for curve in fcurves:
                    keyframePoints = curve.keyframe_points
                    for keyframe in keyframePoints:
                        keyframe.co[0] += offset
                        keyframe.handle_left[0] += offset
                        keyframe.handle_right[0] += offset

            offset += self.offset_frame

        return {'FINISHED'}

class MESH_OT_adh_project_background_image_to_mesh(Operator):
    bl_idname = 'mesh.adh_project_background_image_to_mesh'
    bl_label = 'Project Background Image to Mesh'
    bl_options = {'REGISTER', 'UNDO'}

    bkg_image_idx = EnumProperty(items = get_background_enums)

    invoked = False

    @classmethod
    def poll(self, context):
        return context.space_data.type == 'VIEW_3D'\
            and context.space_data.background_images\
            and context.object and context.object.type == 'MESH'

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return

        row = layout.row()
        row.label('Background Image:')
        row.prop(self, 'bkg_image_idx', text='')

    def execute(self, context):
        cam_list = [o for o in context.selected_objects if o.type == 'CAMERA']
        cam = cam_list[0] if cam_list else None

        obj = context.object
        scene = context.scene
        space = context.space_data
        image = space.background_images[int(self.bkg_image_idx)].image

        prev_perspective = space.region_3d.view_perspective
        prev_camera = scene.camera
        prev_mode = obj.mode
        space.region_3d.view_perspective = 'CAMERA'
        if cam:
            scene.camera = cam
        bpy.ops.object.mode_set(mode = 'EDIT')

        bpy.ops.mesh.select_all(action = 'SELECT')
        bpy.ops.uv.project_from_view(
            orthographic = (scene.camera.type == 'ORTHO'),
            camera_bounds = True, correct_aspect = False,
            clip_to_bounds = False, scale_to_bounds = False)

        bpy.ops.object.mode_set(mode=prev_mode)
        space.region_3d.view_perspective = prev_perspective
        scene.camera = prev_camera

        uvmap = obj.data.uv_textures.active
        uvmap.name = PRJ_UVMAP_PREFIX
        for uvmap_poly in uvmap.data:
            uvmap_poly.image = image

        mat_name = PRJ_MAT_PREFIX + obj.name
        mat = bpy.data.materials.get(mat_name, None)
        if not mat:
            mat = bpy.data.materials.new(mat_name)
        mat.game_settings.alpha_blend = 'ALPHA'
        mat.use_shadeless = True

        tex_name = PRJ_TEX_PREFIX + obj.name
        tex = bpy.data.textures.get(tex_name, None)
        if not tex:
            tex = bpy.data.textures.new(tex_name, 'IMAGE')
        tex.image = image

        texslot = mat.texture_slots.create(0)
        texslot.texture_coords = 'UV'
        texslot.uv_layer = uvmap.name
        texslot.texture = tex

        obj.data.materials.clear()
        obj.data.materials.append(mat)

        return {'FINISHED'}

    def invoke(self, context, event):
        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class VIEW3D_OT_adh_background_image_from_scene(Operator):
    bl_idname = 'view3d.adh_background_image_from_scene'
    bl_label = 'Add Background Image from Scene'
    bl_options = {'REGISTER', 'UNDO'}

    scale = IntProperty(min = 0, max = 100,
                        default = 100, subtype = 'PERCENTAGE')
    scene_name = EnumProperty(items = get_scene_enums)
    dirpath = StringProperty(subtype = 'DIR_PATH', default = "//")
    external_editor = BoolProperty(default = True)
    opengl = BoolProperty(default = True)
    invoked = False

    @classmethod
    def poll(self, context):
        return context.space_data.type == 'VIEW_3D'

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return
        
        row = layout.row()
        row.label("Scene:") ; row.prop(self, 'scene_name', text="")
        row = layout.row()
        row.label("Resolution Percentage:") ; row.prop(self, 'scale', text="")
        row = layout.row()
        row.label("File Location:") ; row.prop(self, 'dirpath', text="")
        row = layout.row()
        row.label("OpenGL:") ; row.prop(self, 'opengl', text=" ")

    def execute(self, context):
        space = context.space_data
        scene = bpy.data.scenes[self.scene_name]
        if not scene.camera:
            self.report({'ERROR'},
                        'Scene "%s" has no active camera.' % self.scene_name)
            return {'CANCELLED'}

        bkg_preview = None
        prv_filename = PRJ_IMG_PREFIX + self.scene_name + '.png'
        prv_filepath = os.path.join(self.dirpath, prv_filename)
        for bkg_image in space.background_images:
            if bkg_image.image and bkg_image.image.filepath == prv_filepath:
                bkg_preview = bkg_image
                continue
            space.background_images.remove(bkg_image)

        render_image(context, prv_filepath, scale = self.scale,
                     scene = scene, opengl = self.opengl)

        if not bkg_preview:
            bkg_preview = space.background_images.new()
            bkg_image = bpy.data.images.load(prv_filepath)
            bkg_image.name = prv_filename
            bkg_preview.image = bkg_image
        else:
            bkg_preview.image.filepath = prv_filepath
            bkg_preview.image.reload()
        bkg_preview.show_expanded = False
        bkg_preview.view_axis = 'CAMERA'

        space.show_background_images = True
        return {'FINISHED'}

    def invoke(self, context, event):
        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class SEQUENCER_OT_adh_add_annotation_image_strip(Operator):
    bl_idname = 'sequencer.adh_add_annotation_image_strip'
    bl_label = 'Add Annotation Image Strip'
    bl_options = {'REGISTER', 'UNDO'}

    scene_name = EnumProperty(items = get_scene_enums)
    filepath = StringProperty(subtype = 'FILE_PATH', default = "//frame.png")
    external_editor = BoolProperty(default = True)
    opengl = BoolProperty(default = True)
    invoked = False

    @classmethod
    def poll(self, context):
        return context.space_data.type == 'SEQUENCE_EDITOR'

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return

        row = layout.row()
        row.label("Scene:") ; row.prop(self, 'scene_name', text="")
        row = layout.row()
        row.label('File Path:') ; row.prop(self, 'filepath', text='')
        row = layout.row()
        row.label("OpenGL:") ; row.prop(self, 'opengl', text=" ")
        row = layout.row()
        row.label('Use External Editor:')
        row.prop(self, 'external_editor', text=' ')

    def execute(self, context):
        self.filepath = bpy.path.abspath(self.filepath)

        render_scene = bpy.data.scenes[self.scene_name]
        render_image(context, self.filepath, scene = render_scene,
                     opengl = self.opengl)
        if not os.path.exists(self.filepath):
            self.report({'ERROR'}, "Render output file doesn't exist.")
            return {'CANCELLED'}

        scene = context.scene
        strip_duration = 50
        topmost_channel = 1
        scene.sequence_editor_create()
        sequences = scene.sequence_editor.sequences
        if scene.sequence_editor:
            frame_current = scene.frame_current
            topmost_channel = get_topmost_channel(
                sequences, frame_current, frame_current + strip_duration)
        image_strip = sequences.new_image(
            os.path.basename(self.filepath), self.filepath,
            topmost_channel + 1, scene.frame_current)
        image_strip.blend_type = 'ALPHA_OVER'
        image_strip.frame_final_duration = strip_duration
        image_strip.mute = True # Unmute manually after edit = reload image

        edit_image_file(context, self.filepath, self.external_editor)
        
        return {'FINISHED'}

    def invoke(self, context, event):
        self.filepath = '//frame_%04d.png' % (context.scene.frame_current)

        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class SEQUENCER_OT_adh_fade_in_out_selected_strips(Operator):
    bl_idname = 'sequencer.adh_fade_in_out_selected_strips'
    bl_label = 'Fade In/Out Selected Strips'
    bl_options = {'REGISTER', 'UNDO'}

    do_fade_in = BoolProperty(default = True)
    do_fade_out = BoolProperty(default = True)
    fade_in_secs = IntProperty(default = 2, min = 0)
    fade_out_secs = IntProperty(default = 2, min = 0)

    invoked = False

    @classmethod
    def poll(self, context):
        return context.selected_sequences

    def draw(self, context):
        layout = self.layout
        if self.invoked:
            return

        row = layout.row()
        row.prop(self, 'do_fade_in', text = '')
        row.label('Fade In:')
        row.prop(self, 'fade_in_secs', text = '')

        row = layout.row()
        row.prop(self, 'do_fade_out', text = '')
        row.label('Fade Out:')
        row.prop(self, 'fade_out_secs', text = '')
    
    def execute(self, context):
        scene = context.scene
        if not scene.animation_data:
            scene.animation_data_create()

        action = scene.animation_data.action
        if not action:
            action = bpy.data.actions.new(scene.name + "_sequence_anim")
            scene.animation_data.action = action

        fps = context.scene.render.fps
        for sequence in context.selected_sequences:
            fcurve = get_fcurve(action, sequence)

            frame_duration = sequence.frame_final_duration
            fade_in_frames = self.fade_in_secs * fps
            fade_out_frames = self.fade_out_secs * fps
            frame_shortage = -frame_duration + (
                (fade_in_frames if self.do_fade_in else 0) +
                (fade_out_frames if self.do_fade_out else 0))                
            if frame_shortage > 0:
                fade_in_frames -= frame_shortage / 2
                fade_out_frames -= frame_shortage / 2

            frame_start = sequence.frame_final_start
            frame_end =  sequence.frame_final_end
            
            for point_x in [p.co.x for p in fcurve.keyframe_points if
                            p.co.x > frame_start and p.co.x < frame_end]:
                point = next(filter(lambda p: p.co.x == point_x,
                                    fcurve.keyframe_points), None)
                fcurve.keyframe_points.remove(point)
            for frame, value in [(frame_start, 0 if self.do_fade_in else 1),
                                 (frame_start + fade_in_frames, 1),
                                 (frame_end - fade_out_frames, 1),
                                 (frame_end, 0 if self.do_fade_out else 1)]:
                point = fcurve.keyframe_points.insert(frame, value)
                point.interpolation = 'LINEAR'

        return {'FINISHED'}

    def invoke(self, context, event):
        retval = context.window_manager.invoke_props_dialog(self)
        self.invoked = True
        return retval

class ImageMixin:
    image = None
    filepath = None
    master_filepath = None

    def init_variables(self, context):
        scene = context.scene
        space = context.space_data
        obj = context.active_object
        seq = context.selected_editable_sequences[0]\
            if context.selected_editable_sequences else None

        if space.type == "IMAGE_EDITOR" and space.image:
            self.image = space.image
            self.filepath = bpy.path.abspath(space.image.filepath)
        elif obj and obj.type == 'MESH' and obj.data.uv_textures.active:
            self.image = get_uvmap_image(obj.data.uv_textures.active)
            if self.image:
                self.filepath = bpy.path.abspath(self.image.filepath)
        elif seq and seq.type == 'IMAGE':
            seq_element = seq.strip_elem_from_frame(scene.frame_current)
            filedir = bpy.path.abspath(seq.directory)
            filename = seq_element.filename
            self.filepath = os.path.join(filedir, filename)

        if self.filepath:
            self.master_filepath = get_master_file(self.filepath)

class IMAGE_OT_adh_external_edit_master(Operator, ImageMixin):
    bl_idname = 'image.adh_external_edit_master'
    bl_label = 'Master Image Edit Externally'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        self.init_variables(context)
        if not self.filepath:
            self.report({"WARNING"}, "No image file within context.")
            return {'CANCELLED'}

        edit_image_file(context, self.master_filepath)
        return {'FINISHED'}

class IMAGE_OT_adh_reload_from_master_file(Operator, ImageMixin):
    bl_idname = 'image.adh_reload_from_master_file'
    bl_label = 'Reload From Master File'
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        if not HAS_XCFTOOLS:
            self.report({"ERROR"}, "Needs 'xcf2png' from xcftools package")
            return {'CANCELLED'}

        self.init_variables(context)
        if not self.filepath:
            self.report({"WARNING"}, "No image file within context.")
            return {'CANCELLED'}

        if self.master_filepath.lower().endswith(".xcf"):
            status, output_str = subprocess.getstatusoutput(
                'xcf2png "%s" -o "%s"' % (self.master_filepath, self.filepath))
            if status != 0:
                self.report({"WARNING"}, output_str.replace("\n", " "))

        if self.image:
            self.image.reload()
        context.area.tag_redraw()
        return {'FINISHED'}

def draw_view3d_background_panel(self, context):
    layout = self.layout

    layout.operator('view3d.adh_background_image_from_scene',
                    text = "Add From Scene")

def draw_sequencer_add_strip_menu(self, context):
    layout = self.layout

    layout.operator_context = 'INVOKE_DEFAULT' # It's changed to EXEC somewhere
    layout.operator('sequencer.adh_add_annotation_image_strip',
                    text = "Annotation Image")

def register():
    bpy.types.VIEW3D_PT_background_image.append(draw_view3d_background_panel)
    bpy.types.SEQUENCER_MT_add.append(draw_sequencer_add_strip_menu)
    bpy.utils.register_module(__name__)

def unregister():
    bpy.types.VIEW3D_PT_background_image.remove(draw_view3d_background_panel)
    bpy.types.SEQUENCER_MT_add.remove(draw_sequencer_add_strip_menu)
    bpy.utils.unregister_module(__name__)

if __name__ == "__main__":
    register()

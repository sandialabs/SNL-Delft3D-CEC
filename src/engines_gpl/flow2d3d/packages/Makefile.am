include $(top_srcdir)/common.am

if WITH_ODA
   BUILD_ODA = flow2d3d_openda
endif

SUBDIRS = \
     data \
     plugin_culvert_c \
     plugin_user \
     io_dol_f \
     io \
     kernel_dd_f \
     kernel \
     manager \
     flow2d3d $(BUILD_ODA)


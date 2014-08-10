struct vsfusbd_Versaloon_param_t
{
	uint8_t ep_out;
	uint8_t ep_in;
	
	bool dbuffer_en;
};

extern const struct vsfusbd_class_protocol_t vsfusbd_Versaloon_class;

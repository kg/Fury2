class Device {
private:
	LPDIRECT3DDEVICE8 _device;
	LPDIRECT3DSURFACE8 _backbuffer;
	vector<LPDIRECT3DTEXTURE8> _textures;
	zeroinit<int> _width;
	zeroinit<int> _height;
	zeroinit<bool> _windowed;

	bool init(int Width, int Height);
	void uninit();

public:
	Device(int Width, int Height);
	~Device();

	bool ready();

	inline LPDIRECT3DDEVICE8 getDevice() {
		return _device;
	}

	void setTexture(Texture* newTexture, int stage = 0);
	Texture* getTexture(int stage = 0);

	void setRenderTarget(Texture* newTarget);
	Texture* getRenderTarget();
	Texture* getDefaultRenderTarget();

	template <class BlendMode> void setBlendMode(int stage = 0);
	template <class MergeMode> void setMergeMode(int stage = 0);

	template <class Type> void setVertexFormat(Type& Vertex);

	template <class Type> void drawTrangles(Type* Vertexes, int Count);
	template <class Type> void drawLines(Type* Vertexes, int Count);
};
package org.eclipse.emf.json;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;

public final class JsonMetadata {

	public static final String JSON_KEY = "key";
	public static final String JSON_KEY_TYPE = "keyType";
	public static final String JSON_METADATA_ANN_URI = "JsonMetadata";
	public static final String JSON_KEYTYPE_COMMAND = "command";
	public static final String JSON_KEYTYPE_SIGNAL = "signal";
	public static final String JSON_KEYTYPE_HEADER = "header";

	private Map<EClass, Map<String, EStructuralFeature>> jsonMetadataCache = new HashMap<EClass, Map<String,EStructuralFeature>>();

	static public JsonMetadata INSTANCE = new JsonMetadata();
	
	private JsonMetadata() {}
	
	public Map<String, EStructuralFeature> getJsonFeatures(EClass eClass) {
		return Collections.unmodifiableMap(getFieldFeatureMap(eClass));
	}
	
	public EStructuralFeature getJsonFeature(String jsonFieldName, EClass eClass) {
		return getFieldFeatureMap(eClass).get(jsonFieldName);
	}
	
	private Map<String, EStructuralFeature> getFieldFeatureMap(EClass eClass) {
		Map<String, EStructuralFeature> map = jsonMetadataCache.get(eClass);
		if (map != null) {
			return map;
		}
		
		synchronized (jsonMetadataCache) {
			map = new HashMap<String, EStructuralFeature>();
			
			for (EStructuralFeature feature : eClass.getEAllStructuralFeatures()) {
				String key = getJsonFieldName(feature);
				map.put(key, feature);
			}
			
			jsonMetadataCache.put(eClass, Collections.unmodifiableMap(map));
		}
		
		return map;
	}
	
	public static String getJsonFieldName(EStructuralFeature feature) {
		String key = EcoreUtil.getAnnotation(feature, JSON_METADATA_ANN_URI, JSON_KEY);
		if (key == null) {
			key = feature.getName();
		}
		
		StringBuilder keyBuilder = new StringBuilder();
		String keyType = EcoreUtil.getAnnotation(feature, JSON_METADATA_ANN_URI, JSON_KEY_TYPE);
		if (keyType != null) {
			if (keyType.equals(JSON_KEYTYPE_COMMAND)) {
				keyBuilder.append('.');
			}
			else if (keyType.equals(JSON_KEYTYPE_SIGNAL)) {
				keyBuilder.append('+');
			}
			else if (keyType.equals(JSON_KEYTYPE_HEADER)) {
				keyBuilder.append('_');
			}
		}
		keyBuilder.append(key);
		
		return keyBuilder.toString();
	}

}
